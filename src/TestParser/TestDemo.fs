// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText.TestParser

open Microsoft.VisualStudio.TestTools.UnitTesting
open MobileOwnership.PolicyText

[<TestClass>]
type TestDemoClass() =

    [<TestMethod>]
    member _.TestPingDemo() =
        let pml = """
        let me = "did:example:from" in
        let ping = 
            fun (receiver: string) ->
                let message = {
                    Type: "https://didcomm.org/trust-ping/2.0/ping",
                    Id: System.Guid.NewGuid(),
                    From: me,
                    Body: {
                        ResponseRequested: true
                    }
                } in 
                    send message on receiver;
                    receive on me with
                    | { Type: "https://didcomm.org/trust-ping/2.0/ping-response" } as response when response.Thid = message.Id -> 
                        response
                end
        in 
            let rec respond = 
                fun () -> begin
                    receive on me with
                    | { Type: "https://didcomm.org/trust-ping/2.0/ping" } as message -> 
                        begin
                        respond() |;
                        if message.Body.ResponseRequested then
                            let response = {
                                Type: "https://didcomm.org/trust-ping/2.0/ping-response",
                                Id: System.Guid.NewGuid(),
                                Thid: message.Id
                            } in
                                send response on message.From
                            end
                        end
                end
            in 
                [
                    respond() |,
                    ping "did:example:to"
                ]            
            end
        end
        end
        """
        //let pml = "x.y.z()"
        let term = Parser.parseText pml
        use file = System.IO.File.Create("ping.json")
        term.WriteJSON(file)
        file.Flush()
        file.Close()
