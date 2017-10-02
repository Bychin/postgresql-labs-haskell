CREATE TABLE Spacecraft(
  id SERIAL PRIMARY KEY,
  name TEXT UNIQUE,
  service_life INT DEFAULT 1000,
  birth_year INT CHECK(birth_year > 0)
);

CREATE TABLE Planet(
  id SERIAL PRIMARY KEY,
  name TEXT UNIQUE,
  distance NUMERIC(5,2)
);

CREATE TABLE Commander(
  id SERIAL PRIMARY KEY,
  name TEXT
);

CREATE TABLE Flight(
  id INT PRIMARY KEY,
  spacecraft_id INT REFERENCES Spacecraft,
  planet_id INT REFERENCES Planet,
  commander_id INT REFERENCES Commander,
  start_date DATE,
  UNIQUE(spacecraft_id, start_date),
  UNIQUE(commander_id, start_date)
);

-- Table Spacecraft
INSERT INTO spacecraft VALUES (1, 'Barbaric Venus',   750, 2045);
INSERT INTO spacecraft VALUES (2, 'Severe Fist',      820, 2047);
INSERT INTO spacecraft VALUES (3, 'Rough Bulldozer',  930, 2040);
INSERT INTO spacecraft VALUES (4, 'Abandoned Snake',  382, 2036);
INSERT INTO spacecraft VALUES (5, 'Restless Autopsy', 545, 2041);
 
-- Table Planet
INSERT INTO planet (id, NAME, distance) VALUES (1,  'Carehigh',    172.01);
INSERT INTO planet (id, NAME, distance) VALUES (2,  'Medredfan',   130.84);
INSERT INTO planet (id, NAME, distance) VALUES (3,  'Dandindox',   13.37);
INSERT INTO planet (id, NAME, distance) VALUES (4,  'Kanron',      3.10);
INSERT INTO planet (id, NAME, distance) VALUES (5,  'Tanway',      75.28);
INSERT INTO planet (id, NAME, distance) VALUES (6,  'Vilakix',     40.57);
INSERT INTO planet (id, NAME, distance) VALUES (7,  'Bioflex',     89.08);
INSERT INTO planet (id, NAME, distance) VALUES (8,  'Goldenflex',  131.70);
INSERT INTO planet (id, NAME, distance) VALUES (9,  'Opeholding',  90.44);
INSERT INTO planet (id, NAME, distance) VALUES (10, 'Tanzone',     52.83);
INSERT INTO planet (id, NAME, distance) VALUES (11, 'Flexplus',    105.87);
INSERT INTO planet (id, NAME, distance) VALUES (12, 'Volholdings', 5.99);
INSERT INTO planet (id, NAME, distance) VALUES (13, 'Hotla',       195.26);
INSERT INTO planet (id, NAME, distance) VALUES (14, 'Trioline',    15.58);
INSERT INTO planet (id, NAME, distance) VALUES (15, 'Stimex',      164.43);
 
-- Table Commander
INSERT INTO commander VALUES (1, 'Johnny Silverbeard');
INSERT INTO commander VALUES (2, 'Salty Ravenbeard');
INSERT INTO commander VALUES (3, 'Henri Shelley');
INSERT INTO commander VALUES (4, 'Bloody Ravenbeard');
INSERT INTO commander VALUES (5, 'Hungry Redblade');
INSERT INTO commander VALUES (6, 'Gold Stoker');
INSERT INTO commander VALUES (7, 'Thomas Silvergrim');
INSERT INTO commander VALUES (8, 'Donna Gull');
INSERT INTO commander VALUES (9, 'Billy Dreadbeard');
INSERT INTO commander VALUES (10, 'James Scarlet');
INSERT INTO commander VALUES (11, 'Ray Scarlet');
 
-- Table Flight
INSERT INTO flight VALUES (1,  4, 2,  8,  '2047-10-28');
INSERT INTO flight VALUES (2,  4, 5,  6,  '2047-12-16');
INSERT INTO flight VALUES (3,  2, 13, 2,  '2048-02-20');
INSERT INTO flight VALUES (4,  1, 6,  2,  '2048-03-02');
INSERT INTO flight VALUES (5,  5, 12, 5,  '2048-03-20');
INSERT INTO flight VALUES (6,  2, 9,  8,  '2048-03-26');
INSERT INTO flight VALUES (7,  1, 14, 9,  '2048-05-01');
INSERT INTO flight VALUES (8,  4, 3,  2,  '2048-05-22');
INSERT INTO flight VALUES (9,  3, 6,  8,  '2048-05-28');
INSERT INTO flight VALUES (10, 4, 6,  7,  '2048-06-04');
INSERT INTO flight VALUES (11, 4, 4,  7,  '2048-06-17');
INSERT INTO flight VALUES (12, 3, 2,  10, '2048-08-10');
INSERT INTO flight VALUES (13, 1, 4,  8,  '2048-08-18');
INSERT INTO flight VALUES (14, 2, 6,  3,  '2048-09-03');
INSERT INTO flight VALUES (15, 2, 5,  10, '2048-10-08');
INSERT INTO flight VALUES (16, 4, 8,  6,  '2048-10-20');
INSERT INTO flight VALUES (17, 3, 9,  6,  '2048-11-23');
INSERT INTO flight VALUES (18, 3, 7,  7,  '2048-12-17');
INSERT INTO flight VALUES (19, 3, 13, 1,  '2048-12-23');
INSERT INTO flight VALUES (20, 2, 10, 5,  '2048-12-28');
