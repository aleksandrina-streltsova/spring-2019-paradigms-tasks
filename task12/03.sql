-- Выведите столицу Малайзии (Malaysia) (в выводе: только название города).
-- (0,5 баллов)
SELECT Name FROM City 
WHERE Id = (SELECT CityId From Capital 
WHERE CountryCode = (SELECT Code From Country 
Where Name = "Malaysia"));
