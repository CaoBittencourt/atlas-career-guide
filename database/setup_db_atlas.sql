
-- db already created on AWS RDS
-- CREATE DATABASE db_atlas_research;
USE atlas;

-- create attributes / items table
CREATE TABLE
    IF NOT EXISTS df_items (
        -- item_id = item's numeric id = row_number
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
        item_employability TINYINT NOT NULL,
        item_ai TINYINT NOT NULL,
        -- factor id is a foreign key
        fk_factor_id TINYINT NOT NULL
        -- , FOREIGN KEY (fk_factor_id) REFERENCES df_factors (pk_factor_id)
    );