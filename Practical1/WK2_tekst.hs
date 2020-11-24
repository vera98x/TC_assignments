-- Stap 1: maak je data type
-- (...)

-- Stap 2: parsen die handel!
-- Denk aan de volgende operators:
-- <$>, <*>
-- listOf, pack, token, greedy, 

-- stap 3: terug schrijven naar een string formaat

Decals = {
    ['0'] = {
        type = 'Normals',                                           -- String
        name0 = '/env/common/decals/lowshore004_normals.dds',       -- String
        name1 = '',                                                 -- String
        scale = { 56, 56, 56 },                                     -- (Int, Int, Int)
        position = { 834, 56, 733 },                                -- (Int, Int, Int)
        euler = { -0, 6, 0 },                                       -- (Int, Int, Int)
        far_cutoff = 545454,                                        -- Int 
        near_cutoff = 0,                                            -- Int 
        remove_tick = 0,                                            -- Int 
    },
    ['1'] = {
        type = 'Normals',
        name0 = '/env/common/decals/int005_normals.dds',
        name1 = '',
        scale = { 32, 32, 32 },
        position = { 664, 72, 863 },
        euler = { -0, 4, 0 },
        far_cutoff = 544545,
        near_cutoff = 0,
        remove_tick = 0,
    }
}