import MacroSamplePackages17
import Foundation
import Swift

let a = 17
let b = 25

let (result, code) = #stringify(a + b)

print("The value \(result) was produced by the code \"\(code)\"")

// Test URL Macro in URLSwiftMacros.swift
let name = "Ömer"
let age = 23
let url = #URL("https://")

#warning("Buraya dikkat et")

class Webservice {
    @AddAsync
    func fetch(completion: (Data) -> Void) {
        completion(Data())
    }
}

/*
 let webservice = Webservice()

 webservice.fetch { data in
     
 }
*/

