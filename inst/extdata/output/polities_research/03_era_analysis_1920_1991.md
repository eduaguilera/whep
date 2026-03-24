# Polities Research Report: 1920-1991

## Era 7: Interwar Period (1920-1938)

### Key Territorial Changes
- **1920**: Treaty of Trianon - Hungary loses 72% of territory
- **1920-22**: Greco-Turkish War, Treaty of Lausanne 1923
- **1921**: Irish Free State
- **1924-32**: Saudi Arabia unification (Hejaz, Nejd, Asir, Al Hasa)
- **1932**: Manchukuo (Japanese puppet state in Manchuria)

### Database Status
All major interwar changes are well captured:
- Hungary: 5 periods tracking Vienna Awards expansion/contraction
- Turkey: Modern borders from 1920 (Treaty of Sèvres/Lausanne)
- Saudi Arabia: 3 periods (1924-1932, 1932-2000, 2000+)
- Manchukuo: MAN-1932-2025 (FT trade entity)

### Findings
1. **CORRECT**: Saudi Arabia tracking is sophisticated:
   - SAU-1924-1932: Pre-unification (Hejaz + Nejd consolidation)
   - SAU-1932-2000: Unified Kingdom
   - SAU-2000-2025: CShapes shows minor boundary change

2. **NOTE**: Manchukuo (MAN-1932-2025) has end year 2025, which seems wrong since
   Manchukuo ceased to exist in 1945 when Japan surrendered. However, checking the
   code prefix "MAN" and FT source - this is a Federico-Tena trade entity where
   end_year NA defaults to 2025. **Should investigate if this needs fixing.**

3. **NOTE**: No separate "Free City of Danzig" polity (1920-1939). COW data has
   Danzig as a transfer: 2,056 km2 from Germany in 1919. The colleague's "Europe
   from 1850" sheet shows Danzig 1919-1937 as a mandate. Since Danzig didn't have
   independent trade statistics in Federico-Tena, it's excluded. **Design choice.**

## Era 8: WWII (1938-1945)

### Key Territorial Changes
- **1938**: Anschluss (Austria into Germany), Munich Agreement (Sudetenland)
- **1938-40**: First/Second Vienna Awards (Hungary gains from Slovakia, Romania)
- **1939**: German annexation of Memel, Bohemia-Moravia protectorate
- **1940**: USSR annexes Baltic states, Bessarabia, N. Bukovina, Karelia
- **1940**: Romania loses: Bessarabia to USSR, N. Transylvania to Hungary,
  S. Dobruja to Bulgaria
- **1940**: Bulgaria gains Southern Dobruja from Romania (Treaty of Craiova)

### Database Status
| Change | Polity Before | Polity After | Tracked? |
|--------|--------------|-------------|----------|
| Anschluss 1938 | AUT-1918-1919 (ended at 1919!) | DEU-1938-1945 | YES |
| Sudetenland 1938 | F51-1918-1938 -> F51-1938-1945 | DEU-1938-1945 | YES |
| Vienna Awards 1938 | HUN-1920-1938 -> HUN-1938-1947 | expanded | YES |
| Baltic annexed 1940 | EST/LVA/LTU-1918/20-1940 | F228-1940-1945 | YES |
| Finland Karelia 1940 | FIN-1917-1940 -> FIN-1940-2025 | F228-1940-1945 | YES |
| S. Dobruja 1940 | ROU-1920-1940 -> ROU-1940-2025 | BGR-1940-2025 | YES |

### Findings
1. **CORRECT**: All major WWII territorial changes are captured. The database
   correctly tracks:
   - Germany expanding (1938-1945 period with Anschluss + Sudetenland)
   - Czechoslovakia shrinking (1938-1945 period = post-Munich)
   - Hungary expanding (1938-1947 = Vienna Awards)
   - Bulgaria gaining territory (1940+ = Dobruja)
   - Romania losing territory (1920-1940 -> 1940+ = shrunk)
   - USSR expanding (1940-1945 = Baltic annexation + Bessarabia + Karelia)

2. **NOTE**: Austria's code is AUT-1918-2025 for the modern entity, but there are
   also AUT-1918-1919 for the immediate post-WWI period. The overlap is because
   AUT-1918-2025 is the aggregate FAOSTAT/M49 entry while AUT-1918-1919 is the
   CShapes-specific period. The aggregation logic handles this correctly.

## Era 9: Post-WWII and Decolonization (1945-1975)

### Key Events
- **1945**: Germany divided into occupation zones; Oder-Neisse line
- **1947**: India/Pakistan partition; Palestine partition plan
- **1948**: Israel independence; Korea split into North/South
- **1949**: PRC proclaimed; Germany split into FRG/GDR; Indonesia sovereignty
- **1954**: Vietnam split at 17th parallel; French Indochina dissolved
- **1956-68**: African decolonization wave (40+ new states)
- **1960**: "Year of Africa" (17 countries gain independence)
- **1967**: Six-Day War (Israel occupies Sinai, Gaza, West Bank, Golan)
- **1971**: Bangladesh independence from Pakistan; UAE formed
- **1975**: Vietnam reunification; Portuguese colonies independent; Western Sahara crisis

### Database Status for Decolonization
Decolonization tracking uses a dual approach:
- **Colonial period**: Tracked via FT entities (e.g., French West Africa, Belgian Congo)
- **Independent state**: Tracked via CShapes/FAOSTAT/M49 entries

Key independence dates verified against COW data:

| Country | DB Start | COW Independence | Match? |
|---------|----------|-----------------|--------|
| India | 1949 (IND) | 1947 | BY DESIGN: 1949 = post-accession |
| Pakistan | 1971 (PAK) | 1947 | BY DESIGN: 1971 = post-Bangladesh |
| Indonesia | 2002 (IDN) | 1945 | BY DESIGN: 2002 = post-E.Timor |
| Ghana | 1957 (GHA) | 1957 | MATCH |
| Algeria | 1831 (DZA) | 1962 | BY DESIGN: 1831 = French colony start |
| Kenya | 1963 (KEN) | 1963 | MATCH |
| Tanzania | 1964 (TZA) | 1961 | BY DESIGN: 1964 = Tanganyika+Zanzibar |
| Zambia | 1964 (ZMB) | 1964 | MATCH |
| Botswana | 1966 (BWA) | 1966 | MATCH |
| Bangladesh | 1971 (BGD) | 1971 | MATCH |
| Mozambique | 1816 (MOZ) | 1975 | BY DESIGN: 1816 = Portuguese period |
| Angola | 1816 (AGO) | 1975 | BY DESIGN: 1816 = Portuguese period |
| Papua New Guinea | 1975 (PNG) | 1975 | MATCH |

### Findings
1. **CORRECT**: The "by design" mismatches are intentional. The polity_codes track
   territorial entities (which may include colonial periods), not just sovereign
   states. When CShapes shows a territorial change at independence, a new polity
   period starts. When there's no territorial change (just sovereignty change), the
   FT colonial entity continues.

2. **IMPORTANT**: Germany post-WWII gap: DEU-1938-1945 ends at 1945, DEU-1990-2025
   starts at 1990. The 1945-1990 period is covered by:
   - West Germany: F78-1949-1990
   - East Germany: F77-1949-1990
   But 1945-1949 (occupation zones) has no dedicated polity. This is correct because
   the occupation zones didn't have independent trade statistics.

3. **CORRECT**: Vietnam tracking is excellent:
   - VNM-1800-1893: Pre-French Indochina
   - (French Indochina: FID-1887-2025 covers the colonial period)
   - NVI-1954-1975: North Vietnam
   - SVI-1954-1975: South Vietnam
   - VNM-1975-2025: Reunified Vietnam
   Gap 1893-1954 covered by French Indochina. Correct.

4. **CORRECT**: Israel/Egypt/Syria tracking captures Six-Day War perfectly:
   - Israel 1948-1967 -> 1967-1979 -> 1979+ (Six-Day War + Camp David)
   - Egypt 1925-1967 -> 1967-1979 -> 1979+ (Sinai loss + return)
   - Syria 1946-1967 -> 1967+ (Golan Heights loss)

5. **CORRECT**: Korea split tracked via:
   - Korea (to 1910): OKO-1800-1910
   - North Korea: PRK-1948-2025
   - South Korea: KOR-1948-2025
   Gap 1910-1948 = Japanese occupation (tracked under Japan polity).

## Era 10: Cold War Territorial Changes (1967-1991)

### Key Events
- **1967**: Six-Day War (covered above)
- **1971**: UAE formation from Trucial States
- **1975**: Western Sahara - Green March; Mauritania/Morocco partition
- **1976**: Indonesia annexes East Timor
- **1979**: Camp David - Sinai returned to Egypt
- **1979**: Mauritania withdraws from Western Sahara; Morocco takes full control
- **1990**: Yemen unification (North + South Yemen)
- **1990**: German reunification (Oct 3)
- **1991**: USSR dissolution (Dec 26)

### Database Status
1. **UAE Formation (1971)**:
   - Individual emirates tracked in FT: Abu Dhabi, Ajman, Fujairah, Ras al Khaimah,
     Sharjah, Umm al Qawain (all ending at 1971)
   - UAE: ARE-1971-2025
   - **CORRECT**: Complete tracking of pre/post federation.

2. **Western Sahara/Morocco/Mauritania (1975-1979)**:
   - Morocco (1958-1975): MAR-1958-1975
   - Morocco (1975-1979): MAR-1975-1979 (includes W.Sahara partition with Mauritania)
   - Morocco (1979+): MAR-1979-2025 (full W.Sahara control)
   - Mauritania (1960-1975): MRT-1960-1975
   - Mauritania (1975-1979): MRT-1975-1979 (Tiris al-Gharbiyya)
   - Mauritania (1979+): MRT-1979-2025 (after withdrawal)
   - **CORRECT**: Excellent tracking of this complex situation.

3. **Indonesia/East Timor**:
   - Indonesia (1969-1976): IDN-1969-1976 (after West Papua)
   - Indonesia (1976-2002): IDN-1976-2002 (after East Timor annexation)
   - Indonesia (2002+): IDN-2002-2025 (after East Timor independence)
   - **CORRECT**: All transitions captured.

4. **Yemen (1990)**:
   - North Yemen: F246-1918-1990
   - South Yemen: F247-1967-1990
   - Yemen (1990-2000): YEM-1990-2000 (CShapes boundary change in 2000)
   - Yemen (2000+): YEM-2000-2025
   - **CORRECT**: The 2000 split is explained in whep_fixes.csv.

5. **German Reunification (1990)**:
   - West Germany: F78-1949-1990
   - East Germany: F77-1949-1990
   - Germany: DEU-1990-2025
   - **CORRECT**.

6. **USSR Dissolution (1991)**:
   All 15 successor states tracked:
   - Russia: RUS-1991-2014 and RUS-2014-2025
   - Ukraine: UKR-1991-2014 and UKR-2014-2025
   - Belarus: BLR-1991-2025
   - Estonia: EST-1991-2025
   - Latvia: LVA-1991-2025
   - Lithuania: LTU-1991-2025
   - Georgia: GEO-1991-2025
   - Armenia: ARM-1991-2025
   - Azerbaijan: AZE-1991-2025
   - Kazakhstan: KAZ-1991-2025
   - Uzbekistan: UZB-1991-2025
   - Turkmenistan: TKM-1991-2025
   - Tajikistan: TJK-1991-2025
   - Kyrgyzstan: KGZ-1991-2025
   - Moldova: MDA-1991-2025
   - **CORRECT**: All successor states present.
