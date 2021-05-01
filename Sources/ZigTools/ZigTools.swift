//import ZigSyntaxChecker
import ZigSyntaxChecker

struct ZigTools {
    var text = "Hello, World!"
  
  func checkSyntax(of source: String) {
//    var x = hi(5)
    let x = add(5, 5)
    print(x)
  }
  
  func test() -> Int32 {
    // Call the C function
    return add(5, 5)
  }
}
