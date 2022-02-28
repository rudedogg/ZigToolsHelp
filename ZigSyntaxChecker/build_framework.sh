cd "$(dirname "$0")"

zig build-lib src/main.zig -dynamic --name ZigSyntaxChecker

xcodebuild -create-xcframework \
    -library libZigSyntaxChecker.dylib -headers include \
    -output ../ZigSyntaxChecker.xcframework

rm libZigSyntaxChecker.dylib