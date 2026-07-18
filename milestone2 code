CREATE DATABASE f1wheels;
CREATE TABLE sessions(
session_key VARCHAR(5) PRIMARY KEY,
circuit_key VARCHAR(5),
country_key VARCHAR(5),
race_type VARCHAR(15),
circuit_name VARCHAR(30),
country_name VARCHAR(30));


CREATE TABLE drivers(
driver_number VARCHAR(2) PRIMARY KEY,
first_name VARCHAR(15),
last_name VARCHAR(15),
acronym VARCHAR(3),
team VARCHAR(20),
driver_country VARCHAR(3));

CREATE TABLE stints(
stint_ID INT PRIMARY KEY,
stint_number INT,
compound VARCHAR(15),
start_lap INT,
end_lap INT,
start_age INT,
end_age INT,
driver VARCHAR(2) NOT NULL,
race_session VARCHAR(5) NOT NULL,
race_results VARCHAR(7) NOT NULL);


CREATE TABLE results(
result_id VARCHAR(7) PRIMARY KEY,
classification VARCHAR(10),
laps INT,
position INT,
points INT,
driver VARCHAR(2),
result_session VARCHAR(5));

ALTER TABLE results
MODIFY COLUMN result_session VARCHAR(5) NOT NULL;

LOAD DATA LOCAL INFILE '/Users/kendallspencer/Downloads/driver.csv'
INTO TABLE drivers
FIELDS TERMINATED BY ',' 
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS; 

LOAD DATA LOCAL INFILE '/Users/kendallspencer/Downloads/stint1.csv'
INTO TABLE results
FIELDS TERMINATED BY ',' 
LINES TERMINATED BY '\n'
IGNORE 1 ROWS; 

LOAD DATA LOCAL INFILE '/Users/kendallspencer/Downloads/thestints.csv'
INTO TABLE stints
FIELDS TERMINATED BY ',' 
LINES TERMINATED BY '\n'
IGNORE 1 ROWS; 

LOAD DATA LOCAL INFILE '/Users/kendallspencer/Downloads/sesh.csv'
INTO TABLE sessions
FIELDS TERMINATED BY ',' 
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS; 

ALTER TABLE stints
ADD CONSTRAINT fk_results
FOREIGN KEY (race_results) 
REFERENCES results(result_id);

ALTER TABLE stints
ADD CONSTRAINT fk_drivers
FOREIGN KEY (driver)
REFERENCES drivers(driver_number);

ALTER TABLE stints
ADD CONSTRAINT fk_sessions
FOREIGN KEY (race_session)
REFERENCES sessions(session_key);

ALTER TABLE results
ADD CONSTRAINT fk_driver
FOREIGN KEY (driver)
REFERENCES drivers(driver_number);

ALTER TABLE results
ADD CONSTRAINT fk_sesh
FOREIGN KEY (result_session)
REFERENCES sessions(session_key);

SELECT stints.compound, drivers.first_name, drivers.last_name
FROM stints
INNER JOIN drivers ON stints.driver = drivers.driver_number
WHERE stint_number = 1
AND driver= 1;

SELECT stints.driver, stints.end_age, stints.compound
FROM stints
INNER JOIN results on stints.race_results = results.result_id
WHERE position = 1;

SELECT *
FROM results
INNER JOIN drivers on results.driver = drivers.driver_number
INNER JOIN sessions on results.result_session = sessions.session_key
WHERE position IN(1,2,3);
