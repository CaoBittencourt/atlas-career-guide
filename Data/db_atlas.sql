-- CREATE ATLAS DATABASE
CREATE DATABASE IF NOT EXISTS db_atlas_2023;

-- OCCUPATIONS ATTRIBUTES TABLE
-- CREATE TABLE
--     IF NOT EXISTS df_attributes (
--         occupation_id INT PRIMARY KEY AUTO_INCREMENT,
--         occupation_soc INT UNIQUE,
--         occupation_name VARCHAR(200) UNIQUE NOT NULL,
--     );
-- OCCUPATIONS WAGE AND EMPLOYMENT TABLE?
-- OCCUPATIONS METRICS TABLE
-- ITEM METRICS TABLE
-- CREATE TABLE
--     IF NOT EXISTS df_items (
--         -- item_category VARCHAR(50) NOT NULL,
--         item_id INT PRIMARY KEY AUTO_INCREMENT,
--         item_name VARCHAR(100) UNIQUE NOT NULL,
--         item_acronym CHAR(2) UNIQUE NOT NULL,
--         factor_id INT NOT NULL,
--         factor_name VARCHAR(50) NOT NULL,
--         factor_acronym CHAR(2) UNIQUE NOT NULL,
--         item_kflex_macro TINYINT NOT NULL,
--         item_kflex_micro TINYINT NOT NULL,
--         item_kflex_micro_intra TINYINT NOT NULL,
--         item_kflex_micro_inter TINYINT NOT NULL,
--         item_kcost TINYINT NOT NULL,
--         item_eta TINYINT NOT NULL,
--         item_employability TINYINT NOT NULL,
--         item_ai TINYINT NOT NULL
--     );
CREATE TABLE
    IF NOT EXISTS df_items (
        -- language?
        -- item_category VARCHAR(50) NOT NULL,
        item_id INT PRIMARY KEY AUTO_INCREMENT,
        item_name VARCHAR(100) UNIQUE NOT NULL,
        item_acronym CHAR(2) UNIQUE NOT NULL,
        factor_id INT NOT NULL,
        factor_name VARCHAR(50) NOT NULL,
        factor_acronym CHAR(2) UNIQUE NOT NULL,
        item_kflex_macro TINYINT NOT NULL,
        item_kflex_micro TINYINT NOT NULL,
        item_kflex_micro_intra TINYINT NOT NULL,
        item_kflex_micro_inter TINYINT NOT NULL,
        item_kcost TINYINT NOT NULL,
        item_eta TINYINT NOT NULL,
        item_employability TINYINT NOT NULL,
        item_ai TINYINT NOT NULL
    );

-- QUESTIONNAIRE ITEMS EXAMPLES TABLE
CREATE TABLE
    IF NOT EXISTS df_examples (
        -- language = language of the text (e.g. en for english, pt for portuguese, es for spanish)
        language CHAR(2) NOT NULL,
        -- item_id = item's numeric id = row_number
        item_id INT NOT NULL,
        -- item = usable names e.g. f1_active_listening (used for operations)
        item VARCHAR(100) NOT NULL,
        -- item_name = pretty names e.g. Active Listening (used for display / plotting)
        item_name VARCHAR(100) NOT NULL,
        -- item_acronym = 2 letter acronym like a chemical element (e.g. Al for Active Listening)
        item_acronym CHAR(2) NOT NULL,
        -- branch / sub_item / area = subareas of competency within a specific item (e.g. Economics => Econometrics, Finance, Macroeconomics),
        branch VARCHAR(50) NOT NULL,
        -- examples for difficulty levels 1 to 7 
        ex_level1 VARCHAR(100) DEFAULT NULL,
        ex_level2 VARCHAR(100) NOT NULL,
        ex_level3 VARCHAR(100) DEFAULT NULL,
        ex_level4 VARCHAR(100) NOT NULL,
        ex_level5 VARCHAR(100) DEFAULT NULL,
        ex_level6 VARCHAR(100) NOT NULL,
        ex_level7 VARCHAR(100) DEFAULT NULL
    );

-- FACTOR LOADINGS TABLE
-- CREATE TABLE IF NOT EXISTS df_loadings ();
-- USERS INFO TABLE
CREATE TABLE
    IF NOT EXISTS df_users_info (
        user_id INT PRIMARY KEY AUTO_INCREMENT,
        first_name VARCHAR(100) NOT NULL,
        last_name VARCHAR(100) NOT NULL,
        -- Concatenate first and last names with a trigger
        full_name VARCHAR(200) NOT NULL,
        birthday DATE,
        -- Date in which the user registered
        register_date DATE NOT NULL DEFAULT CURRENT_DATE,
        last_active DATE NOT NULL DEFAULT CURRENT_DATE,
        -- User's membership status (premium or not)
        membership VARCHAR(7) DEFAULT 'Free',
        -- User's language
        language CHAR(2) NOT NULL
    );

-- add foreign keys fk_dsdsds

-- INSERT INTO
--     df_users_info (
--         first_name,
--         last_name,
--         full_name,
--         birthday
--     )
-- VALUES
--     ('dsds', 'lalala', 'dsds lalala', '2023-07-10');

-- USERS ATTRIBUTES TABLE (ATLAS PROFESSIONAL PROFILES)
-- CREATE TABLE
--     IF NOT EXISTS df_users_attributes (user_id, user_name,);

-- USERS MATCHES TABLE
-- USERS IDEAL PROFILES TABLE
-- CREATE ONE TABLE FOR EACH ASSESSMENT
CREATE TABLE
    IF NOT EXISTS df_users_matches (
        user_id ,
        user_name,
        user_email,
        assessment_name,
        assessment_date,
        occupation_id,
        occupation_name,
        similarity,
        interchangeability
    );

CREATE TABLE
    IF NOT EXISTS df_users_ai (
        user_id,
        user_name,
        user_email,
        assessment_name,
        assessment_date,
        ai_impact,
        ai_impact_desc
        -- ...
    );