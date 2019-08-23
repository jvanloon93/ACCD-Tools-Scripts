PIT18 <- Pittsburgh_PA_MSA(2018)

PIT18 <- subset(PIT18, own_code == 0 | agglvl_code == 44) %>%
  mutate(Benchmark_Type = "Pittsburgh")

ASP18 <- Aspriational_MSAs(2018)

ASP18 <- bind_rows(ASP18)

ASP18 <- subset(ASP18, own_code == 0 | agglvl_code == 44) %>%
  mutate(Benchmark_Type = "Aspirational")

COM18 <- Competitive_MSAs(2018)

COM18 <- bind_rows(COM18)

COM18 <- subset(COM18, own_code == 0 | agglvl_code == 44) %>%
  mutate(Benchmark_Type = "Competitive")

PEER18 <- Peer_MSAs(2018)

PEER18 <- bind_rows(PEER18)
                    
PEER18 <- subset(PEER18, own_code == 0 | agglvl_code == 44) %>%
  mutate(Benchmark_Type = "Peer")

df18 <- rbind(PIT18, ASP18, COM18, PEER18)

Aspriational_FIPs <- c("C1242", "C1446", "C1974", "C3346", "C4266" )

Aspriational_Names <- c("Austin-Round Rock, TX MSA", "Boston-Cambridge-Newton, MA-NH MSA", "Denver-Aurora-Lakewood, CO MSA",
                        "Minneapolis-St. Paul-Bloomington, MN-WI MSA", "Seattle-Tacoma-Bellevue, WA MSA")

Competitive_FIPs <- c("C1258", "C1674","C2690", "C3498","C3798")

Competitive_Names <- c("Baltimore-Columbia-Towson, MD MSA", "Charlotte-Concord-Gastonia, NC-SC MSA", " Indianapolis-Carmel-Anderson, IN MSA",
                       "Nashville-Davidson--Murfreesboro--Franklin, TN MSA", "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD MSA")

Peer_FIPs <- c("C1714", "C1746", "C1982", "C3334", "C4118")

Peer_Names <- c("Cincinnati, OH-KY-IN MSA", "Cleveland-Elyria, OH MSA", "Detroit-Warren_Dearborn, MI MSA",
                "Milwaukee-Waukesha-West Allis, WI MSA","St. Louis, MO-IL MSA")

Pittsburgh_FIPS <- c("C3830")

Pittsburgh_name <- c("Pittsburgh, PA MSA")

FIPS <- c(Aspriational_FIPs, Competitive_FIPs, Peer_FIPs, Pittsburgh_FIPS)

Names <- c(Aspriational_Names, Competitive_Names, Peer_Names, Pittsburgh_name)

GeoLook_Up <- data.frame(FIPS, Names)

IndustryCodes <- distinct(df18, industry_code)

NAICS_ALL <- blscrapeR::niacs

Industry_LookUp <- merge(IndustryCodes, NAICS_ALL)

df18 <- merge(df18, GeoLook_Up, by.x = "area_fips", by.y = "FIPS")

df18 <- merge(df18, Industry_LookUp, by.x = "industry_code", by.y = "industry_code")

write.csv(df18, "Benchmarks183.csv")

