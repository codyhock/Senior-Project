<?php
#this program creates the csv files I will need to run my tests on
#variables ending with "_file" are the files I am reading from
#variables ending in "_handle" are the files I am creating

	$start_year = $argv[1];
	$end_year   = $argv[2];

	if($start_year < 2000) $start_year = 2000;
	if($end_year > 2013)   $end_year   = 2013;

	$score_pattern   = "|<td align=\"right\"  csk=\"(\d*)\">.*\n.*\n.*\n.*\n.*<strong><a href=.*\">(\D*\d*\D*)</a>.*\n.*right\" >(@?).*\n.*\">(\D*\d*\D*)</a>.*\n.*<strong>(\d*).*\n.*\" >(\d*).*\n.*\" >(\d*).*\n.*\" >(\d*).*\n.*\" >(\d*).*\n.*\" >(\d*)|";
	#do I need points when I have it later?
    #$point_pattern   = "|<td align=\"right\"  csk=\"\d*\">.*\n.*<a href=.*\">(\D*\d*\D*)</a>.*\n.*>(\d*)</td>\n.*>(\d*)</td>|";
	$passing_pattern = "|<td align=\"right\"  csk=\"(\d*)\">.*\n.*<a href=.*\">(\D*\d*\D*)</a>.*\n.*\n.*\n.*\n.*>(\d*\.\d)</td>.*\n.*\n.*>(\d*)</td>.*\n.*\n.*>(\d*)</td>.*\n.*\n.*\n.*\n.*\n.*\n.*>(\d*\.\d)</td>|";
	$rushing_pattern = "|<td align=\"right\"  csk=\"(\d*)\">.*\n.*<a href=.*\">(\D*\d*\D*)</a>.*\n.*\n.*\n.*>(\d*)</td>\n.*>(\d*)</td>\n.*\n.*>(\d*\.\d)</td>\n.*>(\d*\.\d)</td>\n.*>(\d*)</td>|";
	$win_pattern	 = "|<td align=\"left\"  csk=\"[0-9]*\">.*\">(\D*\d*\D*)</a>.*\n.*>(\d*)</td>\n.*>(\d*)</td>\n.*>(\d*)</td>\n.*>(\.\d*)</td>\n.*>(\d*)</td>\n.*>(\d*)</td>|";
	$kick_pattern	 = "|<td align=\"right\"  csk=\"[0-9]*\">.*\n.*<a href=.*\">(\D*\d*\D*)</a>.*\n.*>(\d*)</td>\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*>(\d*)</td>\n.*>(\d*)</td>\n.*>(\d*\.\d*).*</td>|";
	$passdef_pattern = "|<td align=\"right\"  csk=\"(\d*)\">.*\n.*<a href=.*\">(\D*\d*\D*)</a>.*\n.*>(\d*)</td>\n.*\n.*\n.*>(\d*\.\d*)</td>\n.*>(\d*)</td>\n.*>(\d*)</td>\n.*\n.*>(\d*)</td>\n.*\n.*\n.*\n.*\n.*\n.*>(\d*\.\d*)</td>\n.*>(\d*\.\d*)</td>\n.*\n.*>(\d*)</td>\n.*\n.*\n.*\n.*\n.*>(-?\d*\.\d*)</td>|";
	$rushdef_pattern = "|<td align=\"right\"  csk=\"(\d*)\">.*\n.*<a href=.*\">(\D*\d*\D*)</a>.*\n.*>(\d*)</td>\n.*\n.*>(\d*)</td>\n.*>(\d*)</td>\n.*\n.*>(\d*\.\d*)</td>\n.*>(\d*\.\d*)</td>\n.*\n.*>(-?\d*\.\d*)</td>|";
	
	$score_heading   = "Week,Team 1,At,Team 2,PtsW,PtsL,YdsW,TOW,YdsL,TOL,\n";
	$passing_heading = "Rank,Team,Cmp%,TD,Int,Y/G,\n";
	$rushing_heading = "Rank,Team,Yds,TD,Y/A,Y/G,Fmb,\n";
    $win_heading	 = "Team,W,L,Tie,Win%,Pts,PtsOther,\n";
	$kick_heading    = "Team,Games,FGA,FGM,FG%,\n";
	$passdef_heading = "Rank,Team,Games,Cmp%,Yds,TDA,Int,Y/G,QB Rating,Sack,EXP,\n";
	$rushdef_heading = "Rank,Team,Games,Yds,TDA,Y/A,Y/G,EXP,\n";

	for($year_to_check = $start_year; $year_to_check <= $end_year; $year_to_check++){

		$current_year = (string)$year_to_check;
		mkdir("$current_year", 0700);
		#mkdir("$current_year/teams", 0700);

		$score_file   = "http://www.pro-football-reference.com/years/" . $current_year . "/games.htm";
        $offense_file = "http://www.pro-football-reference.com/years/" . $current_year . "/";
        $defense_file = "http://www.pro-football-reference.com/years/" . $current_year . "/opp.htm";

		initFile($score_file,   "_scores.csv",  $score_pattern,   $current_year, $score_heading,   10);
		initFile($offense_file, "_rushing.csv", $rushing_pattern, $current_year, $rushing_heading,  7);
		initFile($offense_file, "_passing.csv", $passing_pattern, $current_year, $passing_heading,  6);
        initFile($offense_file, "_wins.csv",    $win_pattern,     $current_year, $win_heading,      7);
		initFile($offense_file, "_kick.csv",	$kick_pattern,	  $current_year, $kick_heading,	    5);
		initFile($defense_file, "_passdef.csv", $passdef_pattern, $current_year, $passdef_heading, 11);
		initFile($defense_file, "_rushdef.csv", $rushdef_pattern, $current_year, $rushdef_heading,  8);
	}

	function initFile($file_type, $handle, $pattern, $year, $heading, $heading_length){
		$handle  = $year . "/" . $year . $handle;
		$matches = getData($file_type, $pattern);
		makeFile($matches, $handle, $heading, $heading_length);
	}

	function getData($file, $pattern){
		$make_file = fopen($file, "r");
		$data = "";
		do {
			$part = fread($make_file, 1024);
			$data = $data . $part;
		} while ($part != "");
		preg_match_all($pattern, $data, $matches);
		fclose($make_file);
		return $matches;
	}

	function makeFile($match, $file_handle, $heading, $total){
		$handle = fopen($file_handle,"w");
		fwrite($handle,$heading);
		$n = 0;
		
		while (@$match[0][$n] != ""){
			//fwrite($handle,$year.",");
			for($i=1;$i<=$total;$i++){
				fwrite($handle,$match[$i][$n].",");
            }
			fwrite($handle,"\n");
            $n++;
		}
		fclose($handle);
	}
?>