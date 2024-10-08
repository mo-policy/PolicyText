// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText.TestParser

open Microsoft.VisualStudio.TestTools.UnitTesting
open MobileOwnership.PolicyText

[<TestClass>]
type TestHashesClass() =

    let data = [
        (Value.Null, "null", "74234E98AFE7498FB5DAF1F36AC2D78ACC339464F950703B8C019892F982B90B");
        (Value.Boolean(false), "false", "FCBCF165908DD18A9E49F7FF27810176DB8E9F63B4352213741664245224F8AA");
        (Value.Boolean(true), "true", "B5BEA41B6C623F7C09F1BF24DCAE58EBAB3C0CDD90AD966BC43A45B44867E12B");
        (Value.String(""), "\"\"", "12AE32CB1EC02D01EDA3581B127C1FEE3B0DC53572ED6BAF239721A03D82E126");
        (Value.String("hello"), "\"hello\"", "5AA762AE383FBB727AF3C7A36D4940A5B8C40A989452D2304FC958FF3F354E7A");
        (Value.String("he\"llo"), "\"he\\\"llo\"", "88FB0035AB8B4725393700BF1D8A49F9E9C481BB20C71F2289988AC7D625B6BB");
        (Value.String("\u0007"), "\"\\u0007\"", "BC4748CF817763F51DC0D0D7034C52D1D6AF3974B1FDC86999BB6D339F3366B4");
        (Value.Integer(0), "0", "5FECEB66FFC86F38D952786C6D696C79C2DBC239DD4E91B46729D73A27FB57E9");
        (Value.Integer(-22), "-22", "33F88A451FD4154FA54953C60636B671CF82CE1228F40C8692E1D985912155AC")
        (Value.Double(-22.5), "-22.5", "BA9297F62279D0B308B9B94E7F8658AC7BD86D311AF9178518991889BF58BE23")
        (Value.Double(123.45e-14),"123.45e-14", "55FCE4330989CFCAF68EF80EF8FA058F847D52A16AB668D37F558821DF8B1595");
        (Value.List([]), "[]", "4F53CDA18C2BAA0C0354BB5F9A3ECBE5ED12AB4D8E11BA873C2F11161202B945");
        (Value.List([ Value.Integer(1); Value.Boolean(false); Value.List([]); ]), "[1,false,[]]", "A26CED0769EC1DA9DF42D0CC810FEAA9ABC251CFFBDB05EE3E447122C9727A98");
        (Value.Map([]), "{}", "44136FA355B3678A1146AD16F7E8649E94FB4FC21FE77E8310C060F61CAAFF8A");
        (Value.Map([("a", Value.Integer(1))]), "{\"a\":1}", "7A4D20AE55B2F98451858CDF846FEFE021D5C5F616B620C6BF012F5C926BA119");
        (Value.Map([("a", Value.Integer(1));("b", Value.Null);]), "{\"a\":1,\"b\":null}", "48BC9B443D24F557F3830718D5DCD656DF298C17F31FD963CFCDB7246DE450E4");
    ]

    [<TestMethod>]
    member _.TestTermHashes() =
        for (term, _, expected) in data do
            match Value.GetHashed(term) with
            | Value.Hashed(_, hash) ->
                let actual = System.Convert.ToHexString(hash)
                if (actual <> expected) then
                    System.Console.WriteLine(actual)
                Assert.IsTrue((actual = expected))
            | _ -> 
                Assert.Fail()

    [<TestMethod>]
    member _.TestParsedHashes() =
        for (_, text, expected) in data do
            let term = Parser.parseTextHashed text
            match term with
            | Value.Hashed(_, hash) ->
                let actual = System.Convert.ToHexString(hash)
                if (actual <> expected) then
                    System.Console.WriteLine(actual)
                Assert.IsTrue((actual = expected))
            | _ -> 
                Assert.Fail()

    [<TestMethod>]
    member _.TestWriteJSON() =
        for (term, _, _) in data do
            use ms = new System.IO.MemoryStream()
            term.WriteJSON(ms)
            ms.Position <- 0
            use sr = new System.IO.StreamReader(ms)
            let text = sr.ReadToEnd()
            let parsedTerm = Parser.parseText text
            Assert.IsTrue((parsedTerm = term))