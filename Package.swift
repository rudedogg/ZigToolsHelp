// swift-tools-version:5.3

import PackageDescription

let package = Package(
  name: "ZigTools",
  products: [
    .library(
      name: "ZigTools",
      targets: ["ZigTools"]),
  ],
  dependencies: [],
  targets: [
    .target(
      name: "ZigTools",
      dependencies: ["ZigSyntaxChecker"])
    ,
	.binaryTarget(
      name: "ZigSyntaxChecker",
      path: "ZigSyntaxChecker.xcframework"
    ),
    .testTarget(
      name: "ZigToolsTests",
      dependencies: ["ZigTools"]
    ),
  ]
)
