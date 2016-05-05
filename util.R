# util.R

setJavaHeapSizeGB  <- function(heapSize){
    # This must be called before the Java-dependent libraries are loaded, such as RWeka.
    # Out-of-memory errors can be hard to identify and are often reported as "envir" errors, which are misleading.
    # Use the traceback() function to trace back through the call stack and identify if the Java Heap size is too small"
    requestedHeapSize  <- paste("-Xmx", heapSize, "g", sep="")
    options( java.parameters = requestedHeapSize ) 
    
}

