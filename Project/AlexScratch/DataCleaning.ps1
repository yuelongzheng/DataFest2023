#Get data
$Path = "C:\Users\alexa\Desktop\Briefcase\ProfessionalDevelopment\DataFest\ThreeCurlsAndAGuy\ThreeCurlsAndAGuy\Project\Data"
$Original = Import-Csv "$Path\questionposts.csv"
$Raw = Get-Content "$Path\questionposts.csv"
$RawNoFirst = $Raw[1..($Raw.Length - 1)]
$First20 = $Raw[1..20]


#Combine newlines
#Add extra to previous line
# $CorrectStartRegex = "^\d+,\w{2},"
# $TempRaw = $First20
# for ($i = 0; $i -lt $TempRaw.Length; $i++) {
#     if ($TempRaw[$i] -notmatch $CorrectStartRegex) {
#         $TempRaw[$i - 1] += $TempRaw[$i]
#     }
# }

# #Filter out any that don't match anymore
# $NoLineSpill = $TempRaw | Where-Object {$_ -match $CorrectStartRegex}

#Assemble
$Result = @()
foreach ($Line in $RawNoFirst) {
    #Get easy variables at start
    $Id = $Line.Split(",",2)[0]
    $StateAbbr = $Line.Split(",",3)[1]
    $QuestionUno = $Line.Split(",",4)[2]
    #Start isolating smaller and smaller pieces to get to date
    $DateLength = 24
    $AfterQuestionUno = $Line.Split($QuestionUno,2)[1]
    $AfterWithExtraCommasRemoved = $AfterQuestionUno.Substring(1).Replace(',,','')
    $AfterWithExtraCommasRemoved = $AfterWithExtraCommasRemoved.Replace(',,','').Trim()
    $EndChunk = $AfterWithExtraCommasRemoved.Substring((max @(0,($AfterWithExtraCommasRemoved.Length - $DateLength))))
    $DateRegex = "(?<Date>\d{1,2}\/\d{1,2}\/\d{1,2} \d{1,2}:\d{1,2})"
    $EndChunk -match $DateRegex | Out-Null
    $CreatedUtc = $Matches.Date
    $PostText = $AfterWithExtraCommasRemoved.Split(",$CreatedUtc")[0].Replace('"','')
    #Assemble
    $Result += [PSCustomObject]@{
        Id = $Id;
        StateAbbr = $StateAbbr;
        QuestionUno = $QuestionUno;
        PostText = $PostText;
        CreatedUtc = $CreatedUtc;
    }
    # $Result += $Object
    # $Object = $null
}

$Result
# $v | Export-Csv "$Path\maybe.csv" -UseQuotes AsNeeded

#iterate to find where it stops matching
for ($i = 1; $i -lt $Result.Length; $i++) {
    if ($Result[$i].Id -notmatch "\d+") {
        $Result[($i-2)..($i+2)]
        break
    }
}