using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
//using System.Threading.Tasks;


namespace csv
{
    class Program
    {
        /// <summary>
        /// Class to store one CSV row
        /// </summary>
        public class CsvRow : List<string>
        {
            public string LineText 
            { 
                get; set; 
            }
        }

        /// <summary>
        /// Class to write data to a CSV file
        /// </summary>
        public class CsvFileWriter : StreamWriter
        {
            public CsvFileWriter(Stream stream) : base(stream)
            {
            }

            public CsvFileWriter(string filename) : base(filename)
            {
            }

            /// <summary>
            /// Writes a single row to a CSV file.
            /// </summary>
            /// <param name="row">The row to be written</param>
            public void WriteRow(CsvRow row)
            {
                StringBuilder builder = new StringBuilder();
                foreach (string value in row)
                {
                    builder.Append(value);
                    builder.Append(',');
                }
                row.LineText = builder.ToString();
                WriteLine(row.LineText);
            }
        }
        /// <summary>
        /// Class to read data from a CSV file
        /// </summary>
        public class CsvFileReader : StreamReader
        {
            public CsvFileReader(Stream stream) : base(stream)
            {
            }

            public CsvFileReader(string filename) : base(filename)
            {
            }

            /// <summary>
            /// Reads a row of data from a CSV file
            /// </summary>
            /// <param name="row"></param>
            /// <returns></returns>
            public bool ReadRow(CsvRow row)
            {
                row.LineText = ReadLine();
                if (String.IsNullOrEmpty(row.LineText))
                    return false;

                int pos = 0;
                int rows = 0;

                while (pos < row.LineText.Length)
                {
                    string value;

                    // Special handling for quoted field
                    if (row.LineText[pos] == '"')
                    {
                        // Skip initial quote
                        pos++;

                        // Parse quoted value
                        int start = pos;
                        while (pos < row.LineText.Length)
                        {
                            // Test for quote character
                            if (row.LineText[pos] == '"')
                            {
                                // Found one
                                pos++;

                                // If two quotes together, keep one
                                // Otherwise, indicates end of value
                                if (pos >= row.LineText.Length || row.LineText[pos] != '"')
                                {
                                    pos--;
                                    break;
                                }
                            }
                            pos++;
                        }
                        value = row.LineText.Substring(start, pos - start);
                        value = value.Replace("\"\"", "\"");
                    }
                    else
                    {
                        // Parse unquoted value
                        int start = pos;
                        while (pos < row.LineText.Length && row.LineText[pos] != ',')
                            pos++;
                        value = row.LineText.Substring(start, pos - start);
                    }

                    // Add field to list
                    if (rows < row.Count)
                        row[rows] = value;
                    else
                        row.Add(value);
                    rows++;

                    // Eat up to and including next comma
                    while (pos < row.LineText.Length && row.LineText[pos] != ',')
                        pos++;
                    if (pos < row.LineText.Length)
                        pos++;
                }
                // Delete any unused items
                while (row.Count > rows)
                    row.RemoveAt(rows);

                // Return true if any columns read
                return (row.Count > 0);
            }
        }

        void WriteTest()
        {
            // Write sample data to CSV file
            using (CsvFileWriter writer = new CsvFileWriter("WriteTest.csv"))
            {
                for (int i = 0; i < 100; i++)
                {
                    CsvRow row = new CsvRow();
                    for (int j = 0; j < 5; j++)
                        row.Add(String.Format("Column{0}", j));
                    writer.WriteRow(row);
                }
            }
        }

        void ReadTest()
        {
            // Read sample data from CSV file
            using (CsvFileReader reader = new CsvFileReader("ReadTest.csv"))
            {
                CsvRow row = new CsvRow();
                while (reader.ReadRow(row))
                {
                    foreach (string s in row)
                    {
                        Console.Write(s);
                        Console.Write(" ");
                    }
                    Console.WriteLine();
                }
            }
        }

        static void WriteTeamRow(CsvFileWriter writer, CsvFileReader reader, CsvRow writerRow, string team, int column)
        {
            for (int k = 0; k <= 32; k++)
            {
                CsvRow Row = new CsvRow();
                reader.ReadRow(Row);
                bool isMatch = false;
                int index = 0;
                foreach (string value in Row)
                {
                    if (team == value)
                        isMatch = true;
                    if (isMatch)
                        if (index >= column)
                            writerRow.Add(value);
                    index++;
                }
                if (isMatch)
                {
                    reader.Close();
                    break;
                }
            }
        }

        static void Main(string[] args)
        {
            /*wins.csv
              Team, W, L, T, Pts, PtsOther, 
               * 
              rushing.csv
              Rushing, RushTD, RushY/A, RushY/G, Fmb
               * 
              passing.csv
              PassCmp%, PassTd, PassTD, PassInt, PassY/G
               * 
              kick.csv
              FGA, FGM, FG%
               * 
              rushdef.csv
              DefRushYds, DefRushTDA, DefRushY/A, DefRushY/G, DefRushEXP
               * 
              passdef.csv
              DefPassCmp%, DefPassYds, DefPassTDA, DefInt, DefPassY/G, DefQBRating, Sack, DefPassEXP*/
            //for (int year = Convert.ToInt32(args[0]); year <= ; year++)
            //{
                string current_year = args[0];

                CsvFileReader wins_reader = new CsvFileReader(current_year+"_wins.csv");
		var linecount = File.ReadAllLines(current_year+"_wins.csv").Count();
	
                CsvFileReader rushing_reader;
                CsvFileReader passing_reader;
                CsvFileReader kick_reader;
                CsvFileReader rushdef_reader;
                CsvFileReader passdef_reader;

                CsvFileWriter writer = new CsvFileWriter(current_year+".csv");

                CsvRow writerRow = new CsvRow();
                writerRow.Add("Team");
                writerRow.Add("W");
                writerRow.Add("L");
                writerRow.Add("T");
                writerRow.Add("Pts");
                writerRow.Add("PtsOther");

                writerRow.Add("RushingYds");
                writerRow.Add("RushTD");
                writerRow.Add("RushY/A");
                writerRow.Add("RushY/G");
                writerRow.Add("Fmb");

                writerRow.Add("PassCmp%");
                writerRow.Add("PassTD");
                writerRow.Add("PassInt");
                writerRow.Add("PassY/G");

                writerRow.Add("FGA");
                writerRow.Add("FGM");
                writerRow.Add("FG%");

                writerRow.Add("DefRush-Yds");
                writerRow.Add("DefRush-TDA");
                writerRow.Add("DefRush-Y/A");
                writerRow.Add("DefRush-Y/G");
                writerRow.Add("DefRush-EXP");

                writerRow.Add("DefPass-Cmp%");
                writerRow.Add("DefPass-Yds");
                writerRow.Add("DefPass-TDA");
                writerRow.Add("Def-Int");
                writerRow.Add("DefPass-Y/G");
                writerRow.Add("Def-QBRating");
                writerRow.Add("Sack");
                writerRow.Add("DefPass-EX");

                writer.WriteRow(writerRow);

                CsvRow junkRow = new CsvRow();
                wins_reader.ReadRow(junkRow);
                string team = "";

                for (int i = 0; i < linecount-1; i++)
                {
                    writerRow = new CsvRow();
                    CsvRow winsRow = new CsvRow();
                    wins_reader.ReadRow(winsRow);

                    int index = 0;
                    foreach (string value in winsRow)
                    {
                        if (index == 0)
                            team = value;

                        if ((index >= 0 && index <= 6) && index != 4)
                            writerRow.Add(value);
                        index++;
                    }
                    rushing_reader = new CsvFileReader(current_year+"_rushing.csv");
                    WriteTeamRow(writer, rushing_reader, writerRow, team, 2);

                    //passing_reader = new CsvFileReader("2013_passing.csv");
                    passing_reader = new CsvFileReader(current_year+"_passing.csv");
                    WriteTeamRow(writer, passing_reader, writerRow, team, 2);

                    kick_reader = new CsvFileReader(current_year+"_kick.csv");
                    WriteTeamRow(writer, kick_reader, writerRow, team, 2);

                    rushdef_reader = new CsvFileReader(current_year+"_rushdef.csv");
                    WriteTeamRow(writer, rushdef_reader, writerRow, team, 3);

                    passdef_reader = new CsvFileReader(current_year+"_passdef.csv");
                    WriteTeamRow(writer, passdef_reader, writerRow, team, 3);

                    writer.WriteRow(writerRow);
                }

                writer.Close();

                wins_reader.Close();
            //}
        }

    }
}
