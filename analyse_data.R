
ctsl <- read.csv("data/ctsl_perTrial.csv")
en  <- read.csv("data/english_perTrial.csv")

# Number of trials in each game
table(ctsl$gameId)

# gameId
# 0881-8    26
# 1535-6    26
# 2269-9    30
# 3743-d    29
# 5082-3    23
# 7120-3    30
# 7597-b    26
# 7680-b    16
# 9173-1    28
# 9178-6    24
# 9458-d    24

# Number of trials in each game
table(en$gameId)

# gameId
# 0049-5    30
# 0108-5    29
# 0363-a    29
# 0460-4    30
# 0834-b    30
# 0837-8    23
# 1884-8     5
# 2113-1    29
# 2180-1    29
# 2220-4    29
# 2334-6     4
# 2341-8    25
# 2429-5    29
# 2436-e     5
# 3157-3    28
# 3187-1    28
# 3281-6    30
# 3421-d     2
# 3571-e    19
# 4372-a    29
# 4382-d    30
# 4762-9    30
# 5003-b    30
# 5231-f    30
# 5235-8    30
# 6191-a     1
# 6326-d    29
# 6362-d    30
# 6460-8    30
# 6611-1     5
# 7030-e    27
# 7175-2    29
# 7276-a     4
# 7353-8    29
# 7368-d    27
# 7658-1    28
# 8085-9    16
# 8313-7    15
# 8450-3    28
# 9279-7    26
# 9368-4    30
# 9549-9    28
# 9706-e    27
# 9834-2    30
# 9994-6    14


# Number of games: 11
length(table(ctsl$gameId))

# Number of games: 45
length(table(en$gameId))