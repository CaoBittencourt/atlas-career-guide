-- db already created on AWS RDS
-- CREATE DATABASE db_atlas_research;
USE atlas;

-- attributes / items table
CREATE TABLE
    IF NOT EXISTS df_items (
        -- pk_item_id = item's numeric id = row_number
        pk_item_id SMALLINT PRIMARY KEY AUTO_INCREMENT,
        -- item_acronym = 2 letter acronym like a chemical element (e.g. Al for Active Listening)
        item_acronym CHAR(2) UNIQUE NOT NULL,
        -- item_tag = usable names e.g. f1_active_listening (used for operations)
        item_tag VARCHAR(250) UNIQUE NOT NULL,
        -- item_name = pretty names e.g. Active Listening (used for display / plotting)
        item_name VARCHAR(250) UNIQUE NOT NULL,
        -- item chemical element 2 letter acronym
        -- coefficients for each item
        item_kflex_macro TINYINT NOT NULL,
        item_kflex_micro TINYINT NOT NULL,
        item_kflex_micro_intra TINYINT NOT NULL,
        item_kflex_micro_inter TINYINT NOT NULL,
        item_kcost TINYINT NOT NULL,
        item_eta TINYINT NOT NULL,
        item_ai TINYINT NOT NULL,
        -- factor id is a foreign key
        item_employability TINYINT NOT NULL,
        fk_factor_id TINYINT NOT NULL,
        FOREIGN KEY (fk_factor_id) REFERENCES df_factors (pk_factor_id)
    );

CREATE TABLE
    IF NOT EXISTS df_items_examples (
        -- item + branch (subitem) are a composite primary key
        -- for there are some branches of knowledge, arts, etc 
        -- that exist in more than one discipline
        -- this also simplifies normalization
        pk_item_id SMALLINT NOT NULL,
        pk_branch_id SMALLINT NOT NULL,
        PRIMARY KEY (pk_item_id, pk_branch_id),
        -- examples for difficulty levels 1 to 7
        -- multi-language support: create an unique id
        -- for each example of each item + branch pair
        -- reference these unique ids in a separate table in the translation sub-schema
        -- then substitute values according to the user's language
        ex_level1 VARCHAR(100) AS (CONCAT (pk_item_id, pk_branch_id, 'ex1')),
        ex_level2 VARCHAR(100) AS (CONCAT (pk_item_id, pk_branch_id, 'ex2')),
        ex_level3 VARCHAR(100) AS (CONCAT (pk_item_id, pk_branch_id, 'ex3')),
        ex_level4 VARCHAR(100) AS (CONCAT (pk_item_id, pk_branch_id, 'ex4')),
        ex_level5 VARCHAR(100) AS (CONCAT (pk_item_id, pk_branch_id, 'ex5')),
        ex_level6 VARCHAR(100) AS (CONCAT (pk_item_id, pk_branch_id, 'ex6')),
        ex_level7 VARCHAR(100) AS (CONCAT (pk_item_id, pk_branch_id, 'ex7'))
    );

-- one language option
-- CREATE TABLE
--     IF NOT EXISTS df_items_examples (
--         pk_item_id SMALLINT NOT NULL,
--         pk_branch_id SMALLINT NOT NULL,
--         -- item + branch (subitem) are a composite primary key
--         -- for there are some branches of knowledge, arts, etc 
--         -- that exist in more than one discipline
--         -- this also simplifies normalization
--         PRIMARY KEY(pk_item_id, pk_branch_id),
--         -- examples for difficulty levels 1 to 7 
--         ex_level1 VARCHAR(100) DEFAULT NULL,
--         ex_level2 VARCHAR(100) NOT NULL,
--         ex_level3 VARCHAR(100) DEFAULT NULL,
--         ex_level4 VARCHAR(100) NOT NULL,
--         ex_level5 VARCHAR(100) DEFAULT NULL,
--         ex_level6 VARCHAR(100) NOT NULL,
--         ex_level7 VARCHAR(100) DEFAULT NULL
--         -- primary key = branches
--     );
-- factors table
CREATE TABLE
    IF NOT EXISTS df_factors (
        pk_factor_id TINYINT PRIMARY KEY AUTO_INCREMENT,
        factor_acronym CHAR(2) UNIQUE NOT NULL,
        factor_name VARCHAR(100) UNIQUE NOT NULL
        -- , qtd_items TINYINT NOT NULL
    );

-- user information table
CREATE TABLE
    IF NOT EXISTS df_users_info (
        pk_user_id INT PRIMARY KEY AUTO_INCREMENT,
        username VARCHAR(250) UNIQUE NOT NULL,
        first_name VARCHAR(100) NOT NULL,
        last_name VARCHAR(250) NOT NULL,
        birthday DATE NOT NULL,
        register_date DATE NOT NULL DEFAULT CURRENT_DATE,
        last_active DATE NOT NULL DEFAULT CURRENT_DATE,
        -- User's membership status (premium or not)
        membership VARCHAR(7) DEFAULT 'Free',
        -- User's language
        country_iso2 CHAR(2) NOT NULL,
        fk_language_id CHAR(2) NOT NULL
        -- fk_language_id is a foreign key to the translation sub-schema
    );

-- one table for each assessment => flexible, normalized, scalable
-- for each assessment with precalculated data (e.g. ai assessment, kflex, etc)
-- create a separate table with the precalculated data
-- and also one assessments table
CREATE TABLE
    IF NOT EXISTS df_matches (
        pk_timestamp DATETIME NOT NULL DEFAULT NOW (),
        pk_user_id INT NOT NULL,
        pk_occupation_id SMALLINT NOT NULL,
        -- fk_occupation_id SMALLINT NOT NULL,
        -- composite key = user + datetime
        PRIMARY KEY (pk_timestamp, pk_user_id, pk_occupation_id)
        -- PRIMARY KEY (pk_user_id, pk_datetime)
        similarity TINYINT NOT NULL,
        interchangeability TINYINT NOT NULL
        -- interchangeability AS fun_interchangeability (similarity) TINYINT NOT NULL
    );

-- users' professional profiles table
CREATE TABLE
    IF NOT EXISTS df_users_items (
        pk_timestamp DATE DEFAULT NOW (),
        pk_user_id INT,
        pk_item_id INT,
        PRIMARY KEY (pk_timestamp, pk_user_id, pk_item_id),
        item_score TINYINT NOT NULL
    );

-- occupations' professional profiles table
-- occupations' information table
CREATE TABLE
    IF NOT EXISTS df_occupations_info (
        pk_occupation_id INT PRIMARY KEY AUTO_INCREMENT,
        -- soc_code SMALLINT UNIQUE NOT NULL,
        occupation_name VARCHAR(200) UNIQUE NOT NULL,
        fk_cluster_id INT
    );

CREATE TABLE
    IF NOT EXISTS df_occupations_items (
        pk_occupation_id INT,
        pk_item_id INT,
        item_score TINYINT NOT NULL,
        PRIMARY KEY (pk_occupation_id, pk_item_id)
        -- , FOREIGN KEY (pk_occupation_id) REFERENCES df_occupations_info (pk_occupation_id),
        -- FOREIGN KEY (pk_item_id) REFERENCES df_items (pk_item_id)
    );

-- occupations' market variables table
CREATE TABLE
    IF NOT EXISTS df_occupations_market (
        pk_date DATE DEFAULT CURRENT_DATE,
        pk_occupation_id INT,
        -- default country = USA (the only country for which we currently have data)
        pk_country_iso2 CHAR(2) DEFAULT 'us',
        PRIMARY KEY (pk_date, pk_occupation_id, pk_country_iso2),
        -- technically speaking, we don't have to have these data
        -- however, it is questionable to add an occupation to our database
        -- without knowing annual wages or employment levels
        annual_wage SMALLINT NOT NULL,
        employment INT DEFAULT NULL,
        -- employment INT NOT NULL,
        -- on the other hand, as demand trends are based upon economic models
        -- with varying pressupositions and results, it is resoanable to assume NULL values as default
        demand_trend SMALLINT DEFAULT NULL,
        -- write a demand trend scale somewhere else and auto-apply this function when inserting data
        -- demand_trend_desc VARCHAR(100) AS (fun_demand_desc(demand_trend) DEFAULT NULL)
        -- FOREIGN KEY (pk_occupation_id) REFERENCES df_occupations_info (pk_occupation_id),
        -- FOREIGN KEY (pk_occupation_id) REFERENCES df_occupations_items (pk_occupation_id)
    );

-- add foreign keys after creating tables
-- mentors tables
-- cases table
-- comments table
-- cases' reactions table