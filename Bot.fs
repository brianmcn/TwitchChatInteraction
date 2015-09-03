let botowner = "lorgon"
let nickname = "lorgonbot"   // must be all lowercase, not lorgonBOT
let channel = "#lorgon"   // don't forget the hash at the front!

// sample of seeing msgs
//:lorgon!lorgon@lorgon.tmi.twitch.tv PRIVMSG #lorgon :working
//:fixxxertv!fixxxertv@fixxxertv.tmi.twitch.tv PRIVMSG #lorgon :very observant of you!
//[22:04:32] [Server thread/INFO]: [Server] hi
//[22:05:00] [Server thread/INFO]: <Lorgon111> text
//[22:05:24] [Server thread/INFO]: [Lorgon111: Game rule has been updated]
// PRIVMSG #lorgon : text to send to chat

let twitchMsgRegex = new System.Text.RegularExpressions.Regex("^:(\w+)![^ ]* PRIVMSG #(?:\w+) :(.*)$")

//TWITCH WHISPER> :lorgon!lorgon@lorgon.tmi.twitch.tv WHISPER lorgonbot :whish
let twitchWhisperRegex = new System.Text.RegularExpressions.Regex("^:(\w+)![^ ]* WHISPER (?:\w+) :(.*)$")

let MINECRAFT_RECORD_NAMES = [|
    "blocks"
    "cat"
    "chirp"
    "far"
    "mall"
    "mellohi"
    "stal"
    "strad"
    "ward"
    "wait"
    "11"
    "13"
    |]


open System.IO
open System.Net
open System.Net.Sockets 
open System.Diagnostics 

let START_MINECRAFT = true
let START_WHISPER = false

// POLL DATA
let mutable currentPollWords = new ResizeArray<string>()
let mutable alreadyVoted = new ResizeArray<string>()  // also used by hangman

// HANGMAN DATA
let LETTERS = List.init 26 (fun i -> char(i + 97))
let mutable puzzle : string[] = null
let mutable availableLetters = LETTERS
let mutable puzzleLettersToFind : Set<char> = set []
let mutable currentLetterVotes : int[] = Array.zeroCreate 26
let mutable voteThresholds = [|8;7;6;5;4|]  // if a letter has N more votes than next best, take it now
let mutable hangmanVotingIsOpen = false

let sanitize(s:string) =
    for c in s do 
        if not(System.Char.IsLetterOrDigit(c)) then 
            failwith "bad data"
    s

let remindFollow(tw : StreamWriter) =
    tw.WriteLine("PRIVMSG " + channel + " : Follow Brian here on twitch, on Twitter @lorgon111, and at https://www.youtube.com/user/lorgon111")
    tw.Flush()

let remindPoll(tw : StreamWriter) =
    let sb = new System.Text.StringBuilder("The options are: [")
    for i = 0 to currentPollWords.Count - 2 do
        sb.Append(currentPollWords.[i]).Append(", ") |> ignore
    sb.Append(currentPollWords.[currentPollWords.Count - 1]).Append("]") |> ignore
    tw.WriteLine("PRIVMSG " + channel + " : Use \"!vote <option>\" to vote. Only your first vote will be counted. " + sb.ToString())
    tw.Flush()

let startPoll(mc : StreamWriter, tw : StreamWriter) =
    mc.WriteLine("/scoreboard objectives add TwitchPoll dummy")
    mc.WriteLine("/scoreboard players reset * TwitchPoll")    // TODO has tons of output if entities in scoreboard, maybe put in cmd block and run instead?
    for w in currentPollWords do
        mc.WriteLine(sprintf "/scoreboard players set %s TwitchPoll 0" (sanitize w))
    mc.WriteLine("/scoreboard objectives setdisplay sidebar TwitchPoll")
    mc.Flush()
    remindPoll(tw)

type InputEvent =
    | CONSOLE of string     // stuff typed into the keyboard console of this program
    | MINECRAFT of string   // the stdout of the Minecraft process
    | TWITCH of string      // the wire messages twitch is sending us
    | TIMER of int          // for hangman puzzle events that happen after some time passes

do //if false then
    // standard producer-consumer pattern... we are concurrently reading events from 3 independent sources (on background threads), and adding them to an event queue.
    // queue is processed serially (on main thread) to ensure that we don't get interleaved writes when we write to the console, to minecraft, or to twitch.
    use inputEvents = new System.Collections.Concurrent.BlockingCollection<_>()
    let scheduleEventInNSeconds(n, e) =
        (new System.Threading.Thread(fun () -> System.Threading.Thread.Sleep(System.TimeSpan.FromSeconds(float n)); inputEvents.Add(e))).Start()

    // SETUP MINECRAFT
    let minecraftStdin =
        let psi = new ProcessStartInfo(UseShellExecute=false, RedirectStandardInput=true, RedirectStandardOutput=true)
        psi.WorkingDirectory <- """C:\Users\brianmcn\Desktop\Server"""
        psi.FileName <- "java"
        psi.Arguments <- "-Xms1024M -Xmx1024M -d64 -jar minecraft_server.15w35e.jar nogui"
        let proc = new Process(StartInfo=psi)
        // START MINECRAFT
        if START_MINECRAFT then
            proc.Start() |> ignore
            let rec rcvloop() =
                let data = proc.StandardOutput.ReadLine()
                if data <> null then
                    inputEvents.Add(MINECRAFT data)
                    rcvloop()
            let t = new System.Threading.Thread(rcvloop)
            t.Start()
        proc.StandardInput

    // SETUP TWITCH
    let twitchInputStream, disp =
        let irc = new TcpClient("irc.twitch.tv", 6667)
        let stream = irc.GetStream()
        let reader = new StreamReader(stream)
        let writer = new StreamWriter(stream)
        writer.WriteLine("PASS " + oauthtoken)
        writer.WriteLine("USER " + nickname + " 0 * :" + botowner)
        writer.WriteLine("NICK " + nickname)
        writer.WriteLine("JOIN " + channel)
        writer.Flush()
        // START TWITCH
        let rec rcvloop() =
            let data = reader.ReadLine()
            if data <> null then
                inputEvents.Add(TWITCH data)
                rcvloop()
        let t = new System.Threading.Thread(rcvloop)
        t.Start()
        writer, {new System.IDisposable with member x.Dispose() = writer.Dispose(); reader.Dispose(); stream.Dispose()}
    use disp = disp

    // SETUP & START CONSOLE
    do
        printfn "press q <enter> to quit, else send twitch irc cmds"
        let rec sendloop() =
            let i = System.Console.ReadLine()
            if i <> "q" then
                inputEvents.Add(CONSOLE i)
                sendloop()
            else
                inputEvents.CompleteAdding()
        let t = new System.Threading.Thread(sendloop)
        t.Start()

    // MAIN LOOP
    for e in inputEvents.GetConsumingEnumerable() do
        match e with
        | MINECRAFT data ->
                try
                    printfn "MINECRAFT> %s" data
                    match data.IndexOf("Lorgon111") with
                    | -1 -> ()
                    | n -> 
                    let data = data.Substring(n+"Lorgon111".Length)
                    match data.IndexOf("> !") with      // may be color reset code between name and text, match separately
                    | -1 -> ()
                    | n -> 
                        let words = data.Substring(n+"> !".Length).ToLowerInvariant().Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries)
                        if words.Length > 0 then
                            match words.[0] with
                            | "follow" -> remindFollow(twitchInputStream)
                            | "pollremind" -> remindPoll(twitchInputStream)
                            | "poll" -> alreadyVoted <- ResizeArray(); currentPollWords <- ResizeArray words.[1..]; startPoll(minecraftStdin, twitchInputStream)
                            | "pollrecord" -> alreadyVoted <- ResizeArray(); currentPollWords <- ResizeArray MINECRAFT_RECORD_NAMES; startPoll(minecraftStdin, twitchInputStream)
                            | "vote" -> 
                                if hangmanVotingIsOpen then
                                    let c = System.Char.ToLowerInvariant(words.[1].[0])
                                    if availableLetters |> List.exists (fun x -> x=c) then
                                        // todo ensure each twitch person can only vote once this round
                                        currentLetterVotes.[int c - 97] <- currentLetterVotes.[int c - 97] + 1
                                        minecraftStdin.WriteLine(sprintf "/scoreboard players add %c TwitchPoll 1" c)
                                        minecraftStdin.Flush()
                            | "hangman" -> 
                                currentPollWords <- ResizeArray() // effectively turns off poll functionality
                                alreadyVoted <- ResizeArray()
                                puzzle <- [|"test"|] //;"stuff";"please"|]  // TODO have multiple puzzles
                                availableLetters <- LETTERS
                                puzzleLettersToFind <- puzzle |> Array.map set |> Set.unionMany 
                                currentLetterVotes <- Array.zeroCreate 26
                                hangmanVotingIsOpen <- true
                                Hangman.drawInitPuzzle(puzzle, minecraftStdin)
                                minecraftStdin.WriteLine("/scoreboard objectives add TwitchPoll dummy")
                                minecraftStdin.WriteLine("/scoreboard players reset * TwitchPoll")    // TODO has tons of output if entities in scoreboard, maybe put in cmd block and run instead?
                                minecraftStdin.WriteLine("/scoreboard objectives setdisplay sidebar TwitchPoll")
                                minecraftStdin.Flush()
                                twitchInputStream.WriteLine("PRIVMSG " + channel + " : Hangman! Use \"!vote <letter>\" to vote for a letter to pick next. Only your first vote this round will be counted.")
                                twitchInputStream.Flush()
                                scheduleEventInNSeconds(10, TIMER 0)
                            | _ -> ()
                with e -> 
                    printfn "MINECRAFT FAULT> %s" (e.ToString())
                    reraise()
        | TIMER n ->
            if n = -1 then
                hangmanVotingIsOpen <- true
                //minecraftStdin.WriteLine("/say Voting for next letter now open.")
                //minecraftStdin.Flush()
                twitchInputStream.WriteLine("PRIVMSG " + channel + " : Voting for next letter now open. Use \"!vote <letter>\" to vote. Only your first vote this round will be counted.")
                twitchInputStream.Flush()
                scheduleEventInNSeconds(10, TIMER 0)
            else
                let mutable pickBestNow = false
                if n < voteThresholds.Length then
                    let vt = voteThresholds.[n]
                    let a = Array.copy currentLetterVotes
                    Array.sortInPlace a
                    if a.[25] - a.[24] > vt then
                        pickBestNow <- true
                else
                    pickBestNow <- true
                if pickBestNow then
                    let m = Array.max currentLetterVotes
                    let i = currentLetterVotes |> Array.findIndex (fun x -> x=m) 
                    let c = char(i+97)
                    alreadyVoted <- ResizeArray()
                    availableLetters <- availableLetters |> List.filter (fun x -> x<>c)
                    puzzleLettersToFind <- Set.remove c puzzleLettersToFind 
                    currentLetterVotes <- Array.zeroCreate 26
                    hangmanVotingIsOpen <- false
                    let found = Hangman.updatePuzzleDraw(c, puzzle, minecraftStdin)
                    minecraftStdin.WriteLine("/scoreboard players reset * TwitchPoll")    // TODO has tons of output if entities in scoreboard, maybe put in cmd block and run instead?
                    if found then
                        minecraftStdin.WriteLine("playsound fireworks.launch @a 0 0 0 0.0 1.0 1.0")
                    else
                        minecraftStdin.WriteLine("/playsound mob.wolf.whine @a 0 0 0 0.0 1.0 1.0")
                    minecraftStdin.WriteLine(sprintf "/say Chat picked '%c' (%d votes) %s" c m (if n < voteThresholds.Length then "due to early large vote lead" else "as best when time expired"))
                    if Set.isEmpty puzzleLettersToFind then
                        for x = 1 to 4 do
                            minecraftStdin.Flush()
                            System.Threading.Thread.Sleep(200)
                            minecraftStdin.WriteLine("/playsound fireworks.launch @a 0 0 0 0.0 1.0 1.0")
                        minecraftStdin.WriteLine("/say Chat solved the puzzle!")
                    else
                        //minecraftStdin.WriteLine("/say Voting closed for 10 seconds so people can re-evaluate the puzzle.")
                        twitchInputStream.WriteLine("PRIVMSG " + channel + " : Voting now closed for 10 seconds so people can re-evaluate the puzzle.")
                        twitchInputStream.Flush()
                        scheduleEventInNSeconds(10, TIMER (-1))
                    minecraftStdin.Flush()
                else
                    if n = voteThresholds.Length-1 then
                        twitchInputStream.WriteLine("PRIVMSG " + channel + " : Only 10 seconds left to vote this round! Use \"!vote <letter>\".")
                        twitchInputStream.Flush()
                    scheduleEventInNSeconds(10, TIMER (n+1))
        | TWITCH data ->
            try
                printfn "TWITCH> %s" data
                if data.StartsWith("PING :tmi.twitch.tv") then
                    twitchInputStream.WriteLine("PONG :tmi.twitch.tv")
                    twitchInputStream.Flush()
                let m = twitchMsgRegex.Match(data)
                if m.Groups.Count = 3 then
                    let user, msg = m.Groups.[1].Value.ToLowerInvariant(), m.Groups.[2].Value.ToLowerInvariant()
                    if hangmanVotingIsOpen then
                        if msg.StartsWith("!vote ") && not(alreadyVoted.Contains(user)) then
                            let c = System.Char.ToLowerInvariant(msg.Substring("!vote ".Length).[0])
                            if availableLetters |> List.exists (fun x -> x=c) then
                                alreadyVoted.Add(user)
                                currentLetterVotes.[int c - 97] <- currentLetterVotes.[int c - 97] + 1
                                minecraftStdin.WriteLine(sprintf "/scoreboard players add %c TwitchPoll 1" c)
                                minecraftStdin.Flush()
                    elif msg.StartsWith("!vote ") && not(alreadyVoted.Contains(user)) && currentPollWords.Contains(msg.Substring("!vote ".Length)) then
                        let v = msg.Substring("!vote ".Length)
                        alreadyVoted.Add(user)
                        minecraftStdin.WriteLine(sprintf "/scoreboard players add %s TwitchPoll 1" (sanitize v))
                        minecraftStdin.WriteLine(sprintf """/tellraw @a {"text":"%s voted for %s","color":"dark_gray"}""" (sanitize user) (sanitize v))
                        minecraftStdin.Flush()
            with e -> 
                printfn "TWITCH FAULT> %s" (e.ToString())
                reraise()
        | CONSOLE data ->
            twitchInputStream.WriteLine(data)  // stuff typed to console goes to twitch
            twitchInputStream.Flush()

    (*
    use whisper_irc = new TcpClient("192.16.64.212", 6667)     // http://twitchstatus.com/ to find group chat server ip   // 192.16.64.212 (Port 6667) 
    use whisper_stream = whisper_irc.GetStream()
    use whisper_reader = new StreamReader(whisper_stream)
    use whisper_writer = new StreamWriter(whisper_stream)
    whisper_writer.WriteLine("PASS " + oauthtoken)
    whisper_writer.WriteLine("USER " + nickname + " 0 * :" + botowner)
    whisper_writer.WriteLine("NICK " + nickname)
    whisper_writer.WriteLine("JOIN " + channel)
    whisper_writer.WriteLine("CAP REQ :twitch.tv/commands")  // to receive whispers
    whisper_writer.Flush()
    System.Threading.Thread.Sleep(500)
    let rec whisper_rcvloop() =
        let data = whisper_reader.ReadLine()
        if data <> null then
            try
                printfn "TWITCH WHISPER> %s" data
                if data.StartsWith("PING :tmi.twitch.tv") then
                    whisper_writer.WriteLine("PONG :tmi.twitch.tv")
                    whisper_writer.Flush()
                let m = twitchWhisperRegex.Match(data)
                if m.Groups.Count = 3 then
                    let user, msg = m.Groups.[1].Value, m.Groups.[2].Value
                    if msg.StartsWith("!echo ") then
                        whisper_writer.WriteLine("PRIVMSG "+channel+" :/w "+user+" you said "+msg)   // TODO unsanitized
                        whisper_writer.Flush()
            with e -> 
                printfn "TWITCH WHISPER FAULT> %s" (e.ToString())
            whisper_rcvloop()
    if START_WHISPER then
        let t = new System.Threading.Thread(whisper_rcvloop)
        t.Start()
    System.Threading.Thread.Sleep(500)
    *)

    // we quit the loop
    printfn "closing..."
printfn "closed"
