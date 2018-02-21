-- -----------------------------------------------------------------------------
-- (c) balarabe@protonmail.com                                 [int_in_words.hs]
-- -----------------------------------------------------------------------------

-- intInWordsEN returns the specified number as a description in words.
-- This function is useful for showing amounts in invoices, etc.
-- 'number' must be a positive integer in the range of 0 to maxBound::Int.
-- E.g. intInWordsEN 256 returns "Two Hundred and Fifty Six"

intInWordsEN :: Int -> String

intInWordsEN num
    | num < 0 || num > (maxBound::Int) = "Overflow"
    | num == 0                         = "Zero"
    | otherwise = tail (
        if (maxBound::Int) > 2147483647 then
            part 6 ++ part 5 ++ part 4 ++ part 3 ++ part 2 ++ part 1 ++ part 0
        else
            part 3 ++ part 2 ++ part 1 ++ part 0
        )

    where part i =
            if n == 0 then "" else (
                showIf hun (ones!!(hun-1) ++ " Hundred") ++

                (if hun > 0 && (ten > 0 || one > 0) then " and" else "") ++

                if ten == 1 && one > 0 then teens!!(one-1)
                    else (  showIf ten (tens!!(ten-1)) ++
                            showIf one (ones!!(one-1))  ) ++

                (if i > 0 then " " else "") ++ groups!!i
            )
            where
                showIf n s = if n > 0 then " " ++ s else ""

                extract num pow10 len =
                    floor ( (n1 - fromIntegral (floor n1) + 10e-5) *
                            10^len )
                        where n1 = fromIntegral(num) / 10^(pow10+len)

                n      = extract num (i*3) 3
                one    = extract n 0 1
                ten    = extract n 1 1
                hun    = extract n 2 1

                ones   = ["One",       "Two",      "Three",
                          "Four",      "Five",     "Six",
                          "Seven",     "Eight",    "Nine"]

                tens   = ["Ten",       "Twenty",   "Thirty",
                          "Forty",     "Fifty",    "Sixty",
                          "Seventy",   "Eighty",   "Ninety"]

                teens  = ["Eleven",    "Twelve",   "Thirteen",
                          "Fourteen",  "Fifteen",  "Sixteen",
                          "Seventeen", "Eighteen", "Nineteen"]

                groups = ["", "Thousand", "Million", "Billion",
                          "Trillion", "Quadrillion", "Quintillion"]

--                                                                  intInWordsEN

{-

tests:

intInWordsEN (-1) == "Overflow"

intInWordsEN 0 == "Zero"

intInWordsEN 1 == "One"

intInWordsEN 256 == "Two Hundred and Fifty Six"

intInWordsEN 1001 == "One Thousand One"

intInWordsEN (10^3)  == "One Thousand"

intInWordsEN (10^6)  == "One Million"

intInWordsEN (10^9)  == "One Billion"

intInWordsEN (10^12) == "One Trillion"

intInWordsEN (10^15) == "One Quadrillion"

intInWordsEN (10^18) == "One Quintillion"

intInWordsEN 1234567 ==
    "One Million \
    \Two Hundred and Thirty Four Thousand \
    \Five Hundred and Sixty Seven"

intInWordsEN 2147483647 ==
    "Two Billion \
    \One Hundred and Forty Seven Million \
    \Four Hundred and Eighty Three Thousand \
    \Six Hundred and Forty Seven"
-- maximum of Int (32-bit)

intInWordsEN 9223372036854775807 ==
    "Nine Quintillion \
    \Two Hundred and Twenty Three Quadrillion \
    \Three Hundred and Seventy Two Trillion Thirty Six Billion \
    \Eight Hundred and Fifty Four Million \
    \Seven Hundred and Seventy Five Thousand"
-- maximum of Int (64-bit)

-}

-- end
