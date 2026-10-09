SQLITE_STATEMENTS = [
    """DROP VIEW IF EXISTS "Social" """,
    """DROP VIEW IF EXISTS "Hobbies" """,
    """DROP VIEW IF EXISTS "Travel" """,
    """DROP VIEW IF EXISTS "Drinks" """,
    """DROP VIEW IF EXISTS "Media" """,
    """DROP VIEW IF EXISTS "Food" """,
    """DROP VIEW IF EXISTS "Dinners" """,
    """DROP VIEW IF EXISTS "Lunches" """,
    """DROP VIEW IF EXISTS "Shops" """,
    """DROP VIEW IF EXISTS "LastUnfinishedBooks" """,
    """DROP VIEW IF EXISTS "LastUnfinishedGames" """,
    """DROP VIEW IF EXISTS "LastPlayedGames" """,
    """DROP VIEW IF EXISTS "LastReadBooks" """,
    """DROP VIEW IF EXISTS "ToPlay" """,
    """DROP VIEW IF EXISTS "UnfinishedGames" """,
    """DROP VIEW IF EXISTS "UnfinishedBooks" """,
    """DROP TABLE IF EXISTS "facts" """,
    """
    CREATE TABLE facts (
        source VARCHAR,
        id VARCHAR,
        rel VARCHAR,
        type VARCAR,
        value VARCHAR
    )
    """,
    """
CREATE VIEW LastPlayedGames AS
WITH
    TitleID AS (
        SELECT "value" as "LastPlayedTitle", "id" as "LastPlayedID"
	    FROM facts
        WHERE "type" = 'Game'
        AND ("rel" = 'Started' OR "rel" = 'Played' OR "rel" = 'Finished')
    ),
    DateID AS (
        SELECT "value" as "PlayedDate", "id" as "PlayedDateID"
        from facts
        WHERE "type" = 'Day'
	)
SELECT
    "LastPlayedTitle" AS Title,
	max("PlayedDate") AS "LastPlayedDate"
FROM
TitleID INNER JOIN DateID
ON "LastPlayedID" = "PlayedDateID"
GROUP BY "LastPlayedTitle"
ORDER BY "LastPlayedDate"
    """,
    """
CREATE VIEW LastReadBooks AS
WITH
    TitleID AS (
        SELECT "value" as "LastReadTitle", "id" as "LastReadID"
	    FROM facts
        WHERE "type" = 'Book'
        AND ("rel" = 'Started' OR "rel" = 'Read' OR "rel" = 'Finished' OR "rel" = 'Listened')
    ),
    DateID AS (
        SELECT "value" as "ReadDate", "id" as "ReadDateID"
        from facts
        WHERE "type" = 'Day'
	)
SELECT
    "LastReadTitle" AS Title,
	max("ReadDate") AS "LastReadDate"
FROM
TitleID INNER JOIN DateID
ON "LastReadID" = "ReadDateID"
GROUP BY "LastReadTitle"
ORDER BY "LastReadDate"
    """,
    """
CREATE VIEW "ToPlay" AS
WITH Games
    AS (SELECT "id",
               "value" AS "Title"
        FROM facts
        WHERE type = 'Game'
        AND "rel" = 'ToPlay'),
    Durations
    AS (SELECT "id",
               Cast(Rtrim("value", 'h') AS INTEGER) AS "Takes Hours"
        FROM facts
        WHERE type = 'Duration'
        AND "rel" = 'Takes')
SELECT "Title",
       "Takes Hours"
FROM Games
       LEFT JOIN Durations
              ON Games.id = Durations.id
ORDER BY "Takes Hours"
    """,
    """
CREATE VIEW UnfinishedGames AS
WITH StartedGames
    AS (SELECT DISTINCT "value" AS "Title"
        FROM facts
        WHERE type = 'Game'
        AND ("rel" = 'Started' OR "rel" = 'Played')),
	FinishedGames
    AS (SELECT DISTINCT "value" AS "Title"
        FROM facts
        WHERE type = 'Game'
        AND "rel" = 'Finished')
SELECT "Title"
FROM StartedGames
EXCEPT
SELECT "Title"
FROM FinishedGames
ORDER BY "Title"
    """,
    """
CREATE VIEW LastUnfinishedGames AS
SELECT
    UnfinishedGames.Title,
    LastPlayedDate
FROM UnfinishedGames
LEFT JOIN LastPlayedGames
ON UnfinishedGames.Title = LastPlayedGames.Title
ORDER BY LastPlayedDate, UnfinishedGames.Title
    """,
    """
CREATE VIEW UnfinishedBooks AS
WITH StartedBooks
    AS (SELECT DISTINCT "value" AS "Title"
        FROM facts
        WHERE type = 'Book'
        AND ("rel" = 'Started' OR "rel" = 'Read' OR "rel" = 'Listened')),
	FinishedBooks
    AS (SELECT DISTINCT "value" AS "Title"
        FROM facts
        WHERE type = 'Book'
        AND "rel" = 'Finished')
SELECT "Title"
FROM StartedBooks
EXCEPT
SELECT "Title"
FROM FinishedBooks
ORDER BY "Title"
    """,
    """
CREATE VIEW LastUnfinishedBooks AS
SELECT
    UnfinishedBooks.Title,
    LastReadDate
FROM UnfinishedBooks
LEFT JOIN LastReadBooks
ON UnfinishedBooks.Title = LastReadBooks.Title
ORDER BY LastReadDate, UnfinishedBooks.Title
    """,
    """
CREATE VIEW Shops AS
WITH
    Dates AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Date' AND type = 'Day'
    ),
    Shops AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel LIKE '%shop%'
    ),
    Cost AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Cost'
    )
SELECT
    Dates.value AS "Day",
    Shops.rel AS "ShoppingKind",
    Shops.type AS "ShopKind",
    Shops.value AS "Shop",
    Cost.type AS "Currency",
    Cost.value AS "Amount"
FROM
    Shops
LEFT JOIN Dates ON Dates.source = Shops.source AND Dates.id = Shops.id
LEFT JOIN Cost ON Dates.source = Cost.source AND Dates.id = Cost.id
ORDER BY Day, substr(Shops.id, 0, instr(Shops.id, '/')), substr(Shops.id, instr(Shops.id, '/') + 1)
    """,
    """
CREATE VIEW Lunches AS
WITH
    Dates AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Date' AND type = 'Day'
    ),
    Lunches AS (
        SELECT source, id, rel, type, value FROM facts WHERE type = 'Lunch'
    ),
    Cost AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Cost'
    ),
    Rating AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Rate' and type = '%'
    ),
    Pic AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Pic'
    )
SELECT
    Dates.value AS "Day",
    Lunches.rel AS "Kind",
    Lunches.value AS "Lunch",
    Cost.type AS "Currency",
    Cost.value AS "Amount",
    Rating.value AS "Rating",
    Pic.value AS "Pic"
FROM
    Lunches
INNER JOIN Dates ON Dates.source = Lunches.source AND Dates.id = Lunches.id
LEFT JOIN Cost ON Dates.source = Cost.source AND Dates.id = Cost.id
LEFT JOIN Rating ON Dates.source = Rating.source AND Dates.id = Rating.id
LEFT JOIN Pic ON Dates.source = Pic.source AND Dates.id = Pic.id
ORDER BY Day, substr(Lunches.id, 0, instr(Lunches.id, '/')), substr(Lunches.id, instr(Lunches.id, '/') + 1)
    """,
    """
CREATE VIEW Dinners AS
WITH
    Dates AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Date' AND type = 'Day'
    ),
    Dinners AS (
        SELECT source, id, rel, type, value FROM facts WHERE type = 'Dinner'
    ),
    Cost AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Cost'
    ),
    Rating AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Rate' and type = '%'
    ),
    Pic AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Pic'
    )
SELECT
    Dates.value AS "Day",
    Dinners.rel AS "Kind",
    Dinners.value AS "Dinner",
    Cost.type AS "Currency",
    Cost.value AS "Amount",
    Rating.value AS "Rating",
    Pic.value AS "Pic"
FROM
    Dinners
INNER JOIN Dates ON Dates.source = Dinners.source AND Dates.id = Dinners.id
LEFT JOIN Cost ON Dates.source = Cost.source AND Dates.id = Cost.id
LEFT JOIN Rating ON Dates.source = Rating.source AND Dates.id = Rating.id
LEFT JOIN Pic ON Dates.source = Pic.source AND Dates.id = Pic.id
ORDER BY Day, substr(Dinners.id, 0, instr(Dinners.id, '/')), substr(Dinners.id, instr(Dinners.id, '/') + 1)
    """,
    """
CREATE VIEW Food AS
WITH
    FoodID AS (
        SELECT id, rel, type, value FROM facts WHERE rel = 'NewAlias' AND type = 'Type' AND value = 'Food' LIMIT 1
    ),
    FoodTypes AS (
        SELECT facts.value
        FROM facts INNER JOIN FoodID
        ON facts.id = FoodID.id
        WHERE facts.rel = 'AddAlias' AND facts.type = 'Type' AND facts.id = FoodID.id
    ),
    Dates AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Date' AND type = 'Day'
    ),
    Foods AS (
        SELECT facts.source, facts.id, facts.rel, facts.type, facts.value
        FROM facts
        INNER JOIN FoodTypes
        ON facts.type = FoodTypes.value
    ),
    Cost AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Cost'
    ),
    Rating AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Rate' and type = '%'
    ),
    Pic AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Pic'
    ),
    WhoBy AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'By'
    )
SELECT
    Dates.value AS "Day",
    strftime("%Y-%V", Dates.value) AS Week,
    CASE strftime("%u", Dates.value)
        WHEN '1' THEN 'Mon'
        WHEN '2' THEN 'Tue'
        WHEN '3' THEN 'Wed'
        WHEN '4' THEN 'Thu'
        WHEN '5' THEN 'Fri'
        WHEN '6' THEN 'Sat'
        WHEN '7' THEN 'Sun'
    END AS Weekday,
    Foods.rel AS "Action",
    Foods.type AS "Type",
    Foods.value AS "Food",
    Cost.type AS "Currency",
    Cost.value AS "Amount",
    Rating.value AS "Rating",
    Pic.value AS "Pic",
    WhoBy.value AS "WhoBy"
FROM
    Foods
INNER JOIN Dates ON Dates.source = Foods.source AND Dates.id = Foods.id
LEFT JOIN Cost ON Dates.source = Cost.source AND Dates.id = Cost.id
LEFT JOIN Rating ON Dates.source = Rating.source AND Dates.id = Rating.id
LEFT JOIN Pic ON Dates.source = Pic.source AND Dates.id = Pic.id
LEFT JOIN WhoBy ON Dates.source = WhoBy.source AND Dates.id = WhoBy.id
ORDER BY Day, substr(Foods.id, 0, instr(Foods.id, '/')), substr(Foods.id, instr(Foods.id, '/') + 1)
    """,
    """
CREATE VIEW Media AS
WITH
    MediaID AS (
        SELECT id, rel, type, value FROM facts WHERE rel = 'NewAlias' AND type = 'Type' AND value = 'Media' LIMIT 1
    ),
    MediaTypes AS (
        SELECT facts.value
        FROM facts INNER JOIN MediaID
        ON facts.id = MediaID.id
        WHERE facts.rel = 'AddAlias' AND facts.type = 'Type' AND facts.id = MediaID.id
    ),
    Dates AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Date' AND type = 'Day'
    ),
    Media AS (
        SELECT facts.source, facts.id, facts.rel, facts.type, facts.value
        FROM facts
        INNER JOIN MediaTypes
        ON facts.type = MediaTypes.value
    ),
    Cost AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Cost'
    ),
    Rating AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Rate' and type = '%'
    ),
    Pic AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Pic'
    ),
    Bookmark AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Bookmark'
    )
SELECT
    Dates.value AS "Day",
    strftime("%Y-%V", Dates.value) AS Week,
    CASE strftime("%u", Dates.value)
        WHEN '1' THEN 'Mon'
        WHEN '2' THEN 'Tue'
        WHEN '3' THEN 'Wed'
        WHEN '4' THEN 'Thu'
        WHEN '5' THEN 'Fri'
        WHEN '6' THEN 'Sat'
        WHEN '7' THEN 'Sun'
    END AS Weekday,
    Media.rel AS "Action",
    Media.type AS "Type",
    Media.value AS "Media",
    Cost.type AS "Currency",
    Cost.value AS "Amount",
    Rating.value AS "Rating",
    Pic.value AS "Pic",
    Bookmark.type AS "BookmarkType",
    Bookmark.value AS "BookmarkValue"
FROM
    Media
INNER JOIN Dates ON Dates.source = Media.source AND Dates.id = Media.id
LEFT JOIN Cost ON Dates.source = Cost.source AND Dates.id = Cost.id
LEFT JOIN Rating ON Dates.source = Rating.source AND Dates.id = Rating.id
LEFT JOIN Pic ON Dates.source = Pic.source AND Dates.id = Pic.id
LEFT JOIN Bookmark ON Dates.source = Bookmark.source AND Dates.id = Bookmark.id
ORDER BY Day, substr(Media.id, 0, instr(Media.id, '/')), substr(Media.id, instr(Media.id, '/') + 1)
    """,
    """
CREATE VIEW Travel AS
WITH
    TravelID AS (
        SELECT id, rel, type, value FROM facts WHERE rel = 'NewAlias' AND type = 'Rel' AND value = 'Travel' LIMIT 1
    ),
    TravelTypes AS (
        SELECT facts.value
        FROM facts INNER JOIN TravelID
        ON facts.id = TravelID.id
        WHERE facts.rel = 'AddAlias' AND facts.type = 'Rel' AND facts.id = TravelID.id
    ),
    Dates AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Date' AND type = 'Day'
    ),
    Travel AS (
        SELECT facts.source, facts.id, facts.rel, facts.type, facts.value
        FROM facts
        INNER JOIN TravelTypes
        ON facts.rel = TravelTypes.value
    ),
    Cost AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Cost'
    ),
    WhoBy AS(
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'By'
    )
SELECT
    Dates.value AS "Day",
    strftime("%Y-%V", Dates.value) AS Week,
    CASE strftime("%u", Dates.value)
        WHEN '1' THEN 'Mon'
        WHEN '2' THEN 'Tue'
        WHEN '3' THEN 'Wed'
        WHEN '4' THEN 'Thu'
        WHEN '5' THEN 'Fri'
        WHEN '6' THEN 'Sat'
        WHEN '7' THEN 'Sun'
    END AS Weekday,
    Travel.rel AS "Action",
    Travel.type AS "Type",
    Travel.value AS "Travel",
    Cost.type AS "Currency",
    Cost.value AS "Amount",
    WhoBy.value AS "WhoBy"
FROM
    Travel
INNER JOIN Dates ON Dates.source = Travel.source AND Dates.id = Travel.id
LEFT JOIN Cost ON Dates.source = Cost.source AND Dates.id = Cost.id
LEFT JOIN WhoBy ON Dates.source = WhoBy.source AND Dates.id = WhoBy.id
ORDER BY Day, substr(Travel.id, 0, instr(Travel.id, '/')), substr(Travel.id, instr(Travel.id, '/') + 1)
    """,
    """
CREATE VIEW Drinks AS
WITH
    Dates AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Date' AND type = 'Day'
    ),
    Drank AS (
        SELECT facts.source, facts.id, facts.rel, facts.type, facts.value
        FROM facts
        WHERE Rel = 'Drank'
    ),
    Cost AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Cost'
    )
SELECT
    Dates.value AS "Day",
    strftime("%Y-%V", Dates.value) AS Week,
    CASE strftime("%u", Dates.value)
        WHEN '1' THEN 'Mon'
        WHEN '2' THEN 'Tue'
        WHEN '3' THEN 'Wed'
        WHEN '4' THEN 'Thu'
        WHEN '5' THEN 'Fri'
        WHEN '6' THEN 'Sat'
        WHEN '7' THEN 'Sun'
    END AS Weekday,
    Drank.type AS "Type",
    Drank.value AS "Amount",
    Cost.type AS "Currency",
    Cost.value AS "Cost"
FROM
    Drank
INNER JOIN Dates ON Dates.source = Drank.source AND Dates.id = Drank.id
LEFT JOIN Cost ON Dates.source = Cost.source AND Dates.id = Cost.id
ORDER BY Day, substr(Drank.id, 0, instr(Drank.id, '/')), substr(Drank.id, instr(Drank.id, '/') + 1)
    """,
    """
CREATE VIEW Social AS
WITH
    SocialID AS (
        SELECT id, rel, type, value FROM facts WHERE rel = 'NewAlias' AND type = 'Rel' AND value = 'Social' LIMIT 1
    ),
    SocialTypes AS (
        SELECT facts.value
        FROM facts INNER JOIN SocialID
        ON facts.id = SocialID.id
        WHERE facts.rel = 'AddAlias' AND facts.type = 'Rel' AND facts.id = SocialID.id
    ),
    Dates AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Date' AND type = 'Day'
    ),
    Social AS (
        SELECT facts.source, facts.id, facts.rel, facts.type, facts.value
        FROM facts
        INNER JOIN SocialTypes
        ON facts.rel = SocialTypes.value
    )
SELECT
    Dates.value AS "Day",
    strftime("%Y-%V", Dates.value) AS Week,
    CASE strftime("%u", Dates.value)
        WHEN '1' THEN 'Mon'
        WHEN '2' THEN 'Tue'
        WHEN '3' THEN 'Wed'
        WHEN '4' THEN 'Thu'
        WHEN '5' THEN 'Fri'
        WHEN '6' THEN 'Sat'
        WHEN '7' THEN 'Sun'
    END AS Weekday,
    Social.rel AS "Action",
    Social.type AS "Type",
    Social.value AS "Who"
FROM
    Social
INNER JOIN Dates ON Dates.source = Social.source AND Dates.id = Social.id
ORDER BY Day, substr(Social.id, 0, instr(Social.id, '/')), substr(Social.id, instr(Social.id, '/') + 1)
    """,
    """
CREATE VIEW Hobbies AS
WITH
    Dates AS (
        SELECT source, id, rel, type, value FROM facts WHERE rel = 'Date' AND type = 'Day'
    ),
    Hobby AS (
        SELECT facts.source, facts.id, facts.rel, facts.type, facts.value
        FROM facts
        WHERE facts.Type = 'Hobby'
    )
SELECT
    Dates.value AS "Day",
    strftime("%Y-%V", Dates.value) AS Week,
    CASE strftime("%u", Dates.value)
        WHEN '1' THEN 'Mon'
        WHEN '2' THEN 'Tue'
        WHEN '3' THEN 'Wed'
        WHEN '4' THEN 'Thu'
        WHEN '5' THEN 'Fri'
        WHEN '6' THEN 'Sat'
        WHEN '7' THEN 'Sun'
    END AS Weekday,
    Hobby.rel AS "Action",
    Hobby.value AS "Hobby"
FROM
    Hobby
INNER JOIN Dates ON Dates.source = Hobby.source AND Dates.id = Hobby.id
ORDER BY Day, substr(Hobby.id, 0, instr(Hobby.id, '/')), substr(Hobby.id, instr(Hobby.id, '/') + 1)
    """,
]
