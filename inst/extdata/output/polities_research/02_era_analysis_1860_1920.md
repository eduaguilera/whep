# Polities Research Report: 1860-1920

## Era 1: Italian Unification (1859-1871)

### Historical Facts
- **1859**: Lombardy ceded by Austria to Sardinia (Treaty of Zurich)
- **1860**: Tuscany, Modena, Parma, Romagna annexed by Sardinia (plebiscites)
- **1860**: Kingdom of Two Sicilies conquered by Garibaldi (Oct-Nov)
- **1861**: Kingdom of Italy proclaimed (Mar 17), capital Turin
- **1866**: Venetia ceded by Austria (Treaty of Vienna, after Austro-Prussian War)
- **1870**: Rome captured from Papal States (Sep 20), completes unification

### Current Database Status
| Polity | Code | Start | End | Status |
|--------|------|-------|-----|--------|
| Duchy Modena | DMO-1800-1860 | 1800 | 1860 | CORRECT |
| Duchy Parma | DPA-1800-1860 | 1800 | 1860 | CORRECT |
| Tuscany | TUS-1800-1860 | 1800 | 1860 | CORRECT |
| Sardinia | SAR-1800-1860 | 1800 | 1860 | CORRECT |
| Kingdom Two sicilies | KIN-1800-1860 | 1800 | 1860 | CORRECT |
| Two Sicilies | TWO-1800-1860 | 1800 | 1860 | SEE NOTE |
| Papal States | PAP-1800-1870 | 1800 | 1870 | CORRECT |
| Italy (to 1919) | ITA-1861-1919 | 1861 | 1919 | CORRECT |
| Italy | ITA-1919-2025 | 1919 | 2025 | CORRECT |

### Findings
1. **NOTE**: "Kingdom Two sicilies" (KIN-1800-1860) and "Two Sicilies" (TWO-1800-1860)
   appear as separate entries. They refer to the same entity. Check if one comes from
   Federico-Tena and the other from a different source mapping. If they represent the
   same polity, one should be consolidated.
   - FT source: "Kingdom Two sicilies" appears in FT data
   - CShapes source: "Two Sicilies" appears via CShapes name mapping
   - **Decision needed**: Are these the same polity or distinct data entities?

2. **CORRECT**: Italy (to 1919) starts 1861. CShapes shows Italy/Sardinia 1861-1919
   with a territorial change in 1919 (South Tyrol, Istria from Austria). The 1866
   Venetia acquisition is NOT tracked as a separate period because CShapes aggregates
   this. The colleague's "Europe from 1850" sheet shows:
   - Italy: 1862-1866 (area 247,268 km2)
   - Italy: 1867-1870 (area 272,048 km2, includes Venetia)
   - Italy: 1871-1885 (area 283,875 km2, includes Rome)
   This suggests there COULD be more granular tracking but the current approach
   groups 1861-1919 as one polity period. **This is acceptable per WHEP methodology**
   as CShapes filters for "relevant" area changes.

3. **NOTE**: The Ionian Islands (ION-1815-1862) correctly ends at 1862 when they
   were ceded to Greece by the UK. Historically the cession was 1864, but COW records
   it as 1862 in a cession entry. **POTENTIAL DATE ISSUE**: Check if 1864 is more
   accurate (Treaty of London, Mar 29, 1864, handover May 1864).

## Era 2: German Unification (1864-1871)

### Historical Facts
- **1864**: Second Schleswig War - Denmark loses Schleswig-Holstein to Prussia/Austria
- **1866**: Austro-Prussian War - North German Confederation formed
  - Prussia annexes: Hanover, Hesse-Kassel, Nassau, Frankfurt (1866)
  - North German Confederation: 1867 (22 states under Prussian leadership)
- **1871**: German Empire proclaimed (Jan 18, Versailles)
  - Bavaria, Württemberg, Baden, Hesse-Darmstadt join
  - Alsace-Lorraine annexed from France (Treaty of Frankfurt)

### Current Database Status
| Polity | Code | Start | End | Status |
|--------|------|-------|-----|--------|
| Germany/Zollverein | GER-1800-2025 | 1800 | 2025 | FT AGGREGATE |
| Germany (to 1919) | DEU-1800-1919 | 1800 | 1919 | CORRECT |
| Germany (1919-1920) | DEU-1919-1920 | 1919 | 1920 | CORRECT |
| Germany (1920-1938) | DEU-1920-1938 | 1920 | 1938 | CORRECT |

### Findings
1. **DESIGN CHOICE**: Individual German states before unification (Hanover, Bavaria,
   Saxony, Württemberg, Baden, etc.) are NOT tracked as separate polities. The
   colleague's "Europe from 1850" sheet shows 20+ German states with detailed periods
   (Anhalt-Bernberg 1816-1864, Anhalt-Dessau 1816-1863, Baden 1819-1870, Bavaria
   1827-1870, Bremen 1816-1870, etc.). However, Federico-Tena treats all pre-1871
   German trade under "Germany/Zollverein" because that's how trade data was reported.
   **No change needed** unless the project wants sub-national resolution.

2. **IMPORTANT**: Denmark (DNK-1800-1920) doesn't capture the 1864 loss of
   Schleswig-Holstein (~38,500 km2, about 50% of Denmark's pre-war territory).
   CShapes shows Denmark with separate periods:
   - Denmark to 1864: area 57,086 km2
   - Denmark 1865-1885: area 38,483 km2
   - Denmark 1886-1919: area 38,560 km2
   - Denmark 1920+: area 42,498 km2 (regains N. Schleswig)
   However, the current polity_codes only has DNK-1800-1920 and DNK-1920-2025.
   **The 1864 split is significant (33% area loss) and should be tracked.**

   **Recommendation**: Split DNK-1800-1920 into:
   - Denmark (to 1864): DNK-1800-1864
   - Denmark (1864-1920): DNK-1864-1920
   - Denmark (1920-2025): DNK-1920-2025 (already exists)

3. **NOTE**: Alsace-Lorraine (1871-1918) is not tracked as a separate polity. It was
   a Reichsland (imperial territory) of the German Empire. Since it never had
   independent trade data, this is correct.

## Era 3: Balkans Wars and Independence (1878-1913)

### Historical Facts
- **1878**: Congress of Berlin:
  - Serbia: independent (from Ottoman Empire), area ~48,300 km2
  - Romania: independent, area ~130,000 km2
  - Montenegro: independent, area ~4,400 km2
  - Bulgaria: autonomous principality (~63,000 km2)
  - Bosnia-Herzegovina: Austrian administration (~51,000 km2)
  - Cyprus: British administration
- **1885**: Eastern Rumelia unites with Bulgaria
- **1908**: Austria-Hungary formally annexes Bosnia-Herzegovina; Bulgaria declares full independence
- **1912-1913**: Balkan Wars:
  - First Balkan War: Serbia, Greece, Bulgaria, Montenegro vs Ottoman Empire
  - Second Balkan War: Romania, Serbia, Greece, Ottoman Empire vs Bulgaria
  - Treaty of London (1913): Albania created
  - Treaty of Bucharest (1913): Territory redistribution

### Current Database Status
Balkans tracking is EXCELLENT with detailed period breakdowns for Serbia (4 periods),
Bulgaria (5 periods), Romania (6 periods), Montenegro (3 periods), Greece (3 periods).

### Findings
1. **CORRECT**: Bulgaria starts at 1878 (autonomous), tracks 1913 Balkan Wars
   expansion, 1918-1919 post-WWI, 1919-1940 Treaty of Neuilly, 1940+ (Southern
   Dobruja regained from Romania). All transitions match CShapes.

2. **CORRECT**: Austria-Hungary has two periods:
   - AUH-1800-1908: Before Bosnia annexation
   - AUH-1908-1918: After Bosnia annexation (area increases ~52,000 km2)
   This matches CShapes precisely.

3. **IMPORTANT**: Greece (GRC-1800-1913) starts at 1800 (FT default) but Greece
   became independent in 1830. CShapes starts Greece at 1835. The start year is
   an artifact of Federico-Tena having no start year and defaulting to 1800.
   **This is by design** (FT default) but worth noting.

4. **CORRECT**: Crete (CRE-1898-1913) tracked correctly as autonomous state
   1898-1913 before union with Greece. Matches COW: Crete ceded to UK 1898,
   then annexed to Greece 1913.

## Era 4: Scramble for Africa (1884-1914)

### Historical Facts
- **1884-85**: Berlin Conference establishes rules for African colonization
- Major colonial territories established by 1914 (all tracked in FT data)

### Current Database Status
Extensive colonial polity tracking through Federico-Tena source. Key colonies:
- Belgian Congo (BEL-1885-2025)
- German East Africa (GEA-1884-2025)
- German West Africa (GWA-1884-1914)
- British East Africa (BEA-1895-2025)
- French West Africa (FWA-1895-2025)
- French Equatorial Africa (FEA-1910-2025)

### Findings
1. **CORRECT**: Colonial boundaries well-tracked. The colleague's "Colonies_ft_cshapes"
   sheet confirms all major colonial territories are represented.

2. **NOTE**: Some colonial codes use the prefix of the modern successor state while
   others use artificial codes. E.g., BEL is used for both Belgium and Belgian Congo.
   This is a known prefix collision (see Technical Report).

## Era 5: Ottoman Empire Decline and WWI Aftermath (1911-1920)

### Historical Facts
- **1911**: Italy conquers Libya from Ottoman Empire
- **1912-13**: Balkan Wars - Ottoman loses most European territories
- **1914-18**: WWI
- **1918-20**: Treaty of Versailles, Saint-Germain, Trianon, Neuilly, Sèvres
  - Austria-Hungary dissolved -> Austria, Hungary, Czechoslovakia, Yugoslavia
  - Ottoman Empire -> Turkey, mandates (Syria, Lebanon, Palestine, Iraq, Transjordan)
  - Germany loses: Alsace-Lorraine, Polish Corridor, colonies
  - Russia loses: Finland, Baltic states, Poland, Bessarabia

### Current Database Status
WWI aftermath extremely well tracked:
- Austria-Hungary: 2 periods (to 1908, 1908-1918) -> dissolution tracked
- Germany: 4 periods (to 1919, 1919-1920, 1920-1938, 1938-1945)
- Poland: 5 periods (1918-1919, 1919-1920, 1920-1921, 1921-1945, 1945+)
- Czechoslovakia: 4 periods (1918-1938, 1938-1945, 1945-1947, 1947-1993)
- Yugoslavia: 4 periods (1918-1919, 1919-1920, 1920-1991, 1991-1992)
- Finland: 2 periods (1917-1940, 1940+)
- Baltic states: Each with interwar and post-Soviet periods

### Findings
1. **CORRECT**: All major WWI territorial changes tracked. The level of detail
   (especially for Poland with 5 periods) reflects CShapes' granular boundary data.

2. **CORRECT**: Ireland (IRL-1921-2025) starts 1921 (Anglo-Irish Treaty Dec 1921,
   Irish Free State Jan 1922). CShapes uses 1921. Correct.

3. **NOTE**: Lithuania shows interesting tracking:
   - LTU-1918-1920: Initial independence period
   - LTU-1920-1940: After Vilnius seizure by Poland (lost ~30,000 km2)
   CShapes: Lithuania 1918-1920 = 83,501 km2, Lithuania 1920-1940 = 55,600 km2.
   This correctly captures the Vilnius dispute.

## Era 6: Asia-Pacific Changes (1860-1920)

### Historical Facts
- **1893**: Durand Line - Afghanistan/British India boundary
- **1893-1909**: Thailand loses territories to France and Britain
- **1895**: Treaty of Shimonoseki - Japan gains Taiwan from China
- **1905**: Treaty of Portsmouth - Japan gains S. Sakhalin from Russia
- **1910**: Japan annexes Korea

### Current Database Status
| Polity | Periods | Territorial Changes Tracked |
|--------|---------|---------------------------|
| Afghanistan | 2 (to 1888, 1919+) | Pre/post British period |
| Thailand | 5 (to 1893, 1893-1904, 1904-1907, 1907-1909, 1909+) | French/British cessions |
| China | 8 periods | Shimonoseki through PRC |
| Taiwan | TWN-1896-2025 | Japanese rule through present |
| Korea (to 1910) | OKO-1800-1910 | Pre-annexation |

### Findings
1. **IMPORTANT**: Afghanistan gap 1888-1919 (31 years). CShapes shows:
   - Afghanistan to 1888: 809,279 km2
   - Afghanistan from 1919: 642,119 km2 (post-independence, post-Durand Line)

   COW data shows: Afghanistan cedes 104,980 km2 to UK in 1893 (Durand Line).
   The colleague's whep_equivalences has entries for Afghanistan 1888-1893 and
   1893-1919 but no CShapes geometry for this period.

   **This gap is intentional**: CShapes has no data between 1888-1919 for
   Afghanistan (it was a buffer state between British India and Russia, not
   fully sovereign in CShapes' definition).

2. **CORRECT**: Thailand (Siam) is one of the best-tracked polities with 5 periods
   covering each major territorial cession to France and Britain. Matches both
   CShapes and COW data precisely.

3. **CORRECT**: China's 8 periods meticulously track every significant territorial
   change from 1895 (loss of Taiwan) through 1950 (PRC consolidation).
