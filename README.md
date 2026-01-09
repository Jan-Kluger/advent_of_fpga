## AOC HARDCAML

I have very limited experience in hardware coding. This repo documents my learning journey.

I really enjoyed some of the functional features of Hardcaml, especially the use of the type checker which caught many errors at compile time that would have been runtime bugs in Verilog. The hierarchical module feature was also very useful for organizing complex designs.

Day one was a lot more challenging than expected, so I decided to solve all challenges in normal code first, and then translate my solutions to hardcaml afterward.

Since I had never used hardcaml, I found myself using AI a lot along the way as the questions became quite challenging and hard for me to wrap my head around.

### Day 1

This was my first day doing hardcaml. I hadn't touched hardware design since my 2nd semester in university, so I had to jog my memory a bit. For the first day I really just focused on getting something working, not anything big or efficient, as long as it got a result I was happy. Luckily the challenge for day 1 was pretty easy and I got into it pretty quick. Part 2 was the same.

### Day 2

After doing day 1, I looked back at the Tech Blog by Jane Street. Specifically I wanted to look at the requirements for the challenge, one of them being employing hardware specific optimizations. I didn't think I was ready for hardcaml specific optimizations, but I tried looking into some things. For hardware there were two things that really seemed important to me: 1. Massive parallelization, 2. Pipelining. Both of which I tried implementing.

On day 2, I realized I had been passing input incorrectly. This led to me spending a lot of time with AI trying to find some division algorithm that would work well with hardware. From here on out I have avoided division in my solutions as much as possible.

### Day 3

One of the biggest difficulties I had when doing the past two days was working digit by digit with the inputs. I always resorted to some way of encoding integers, like digit arrays or binary encoded digits, but nothing seemed really elegant to me. I looked more into how I could improve my performance. This led to me realizing I was passing the information to my FPGA completely wrong and I was killing performance. I had to stream information digit by digit.

Day 3 was not easy. I had a lot of difficulties implementing the monotonic stack for part 2. In part 1, I tried to remember back to the pipelining FSM from my computer architecture class, and how in RISC-V processors we can still implement pipelining but we need to remember to sometimes forward information to the other steps. I made sure to implement this and other optimizations today.

Day 3 was definitely the day of optimizations.

### Day 4

I tried to implement a sliding window. The first approach worked well after some difficulties. However, I wasn't really satisfied with the performance of the code and decided to do some more optimizations. The most difficult one was getting it to work in a "streaming" sort of way, where I didn't have to allocate a buffer taking the entire input and then work with the input, but it could just be streamed to the FPGA through UART and spit out a result, saving a lot of memory. This was buggy and took me ALL day, but it finally seems to work.
