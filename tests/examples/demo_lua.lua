-- README.md example for lua-midi
-- Tests the quick example from the project README

open()

-- Concurrent voices with coroutines
spawn(function()
    for _, p in ipairs(scale(c4, "pentatonic")) do
        play(p, mf, eighth)
    end
end, "melody")

spawn(function()
    ch(major(c3), ff, whole)  -- sustained chord
end, "harmony")

run()  -- wait for all voices
close()
