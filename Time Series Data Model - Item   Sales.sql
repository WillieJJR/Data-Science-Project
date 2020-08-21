SELECT a.[Stock Item Key], b.[Stock Item],
SUM(a.Quantity) AS 'SUM of Quantity Sold' ,
CAST(MONTH([Invoice Date Key]) AS VARCHAR(2)) + '-' + CAST(YEAR([Invoice Date Key]) AS VARCHAR(4)) AS MonthYearDate
FROM [WideWorldImportersDW].[Fact].[Sale] a 
INNER JOIN Dimension.[Stock Item] b
ON a.[Stock Item Key] = b.[Stock Item Key]
WHERE a.[Stock Item Key] = 28
GROUP BY a.[Stock Item Key], CAST(MONTH([Invoice Date Key]) AS VARCHAR(2)) + '-' + CAST(YEAR([Invoice Date Key]) AS VARCHAR(4)) , b.[Stock Item]
ORDER BY MonthYearDate