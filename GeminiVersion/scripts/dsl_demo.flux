print("--- DSL Loading ---")

// 1. THE "LIBRARY" CODE
// --------------------
// This is how a developer defines the DSL vocabulary.

var context = "Root"

fun GenericMenu(title: String, builder) {
    print("Building Menu: " + title)
    val previous = context
    context = title
    
    // Execute the 'builder' function (the block passed in)
    builder()
    
    context = previous
    print("Finished Menu: " + title)
}

fun Item(label: String) {
    print("  [" + context + "] Button: " + label)
}

// 2. THE "USER" CODE
// ------------------
// This is how the DSL is consumed. 
// Look how declarative it is!

GenericMenu("Main Settings") {
    Item("Video")
    Item("Audio")
    
    GenericMenu("Controls") {
        Item("Invert Y")
        Item("Sensitivity")
    }
    
    Item("Back")
}
