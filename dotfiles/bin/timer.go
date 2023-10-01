//usr/bin/go run $0 $@; exit
package main

import (
    "flag"
    "time"
    "log"
    "fmt"
)

func main() {
    flag.Parse()
    // Accumurate all args to one value
    var timerTime time.Duration
    for _, s := range flag.Args() {
        if d, err := time.ParseDuration(s); err != nil {
            log.Fatalf("Failed to parse \"%s\": invalid Duration format: %w", s, err)
        } else {
            timerTime += d
        }
	}

    fmt.Printf("timer activated for %v ", timerTime)

   	timer := time.NewTimer(timerTime)
   	_ = <-timer.C
   	fmt.Println("done")
}
