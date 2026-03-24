# Polities Research Report: 1991-Present

## Era 11: Post-Cold War (1991-2008)

### Key Events
- **1991-92**: Yugoslavia breakup (Slovenia, Croatia, Bosnia, Macedonia)
- **1992-95**: Bosnian War
- **1993**: Czechoslovakia dissolution (Czech Republic + Slovakia)
- **1993**: Eritrea independence from Ethiopia
- **1997**: Hong Kong handover (UK to China)
- **1999**: Macau handover (Portugal to China)
- **2002**: East Timor (Timor-Leste) independence
- **2006**: Montenegro independence from Serbia
- **2008**: Kosovo declaration of independence

### Database Status
| Event | Polities | Correct? |
|-------|----------|----------|
| Slovenia independence | SVN-1992-2025 | YES |
| Croatia independence | HRV-1992-2025 | YES |
| Bosnia independence | BIH-1992-2025 | YES |
| Macedonia independence | MKD-1991-2025 | YES |
| Czechoslovakia split | CZE-1993-2025, SVK-1993-2025 | YES |
| Eritrea independence | ERI-1882-2025 (colony start), ETH-1993-2025 | YES |
| East Timor independence | TLS-1800-2025, IDN-2002-2025 | YES |
| Montenegro independence | MNE-2006-2025 | YES |
| Serbia post-Montenegro | SER-2006-2008 | YES |
| Kosovo independence | KOS-2008-2025 | YES |
| Serbia post-Kosovo | SRB-2008-2025 | YES |
| Serbia and Montenegro | SCG-1992-2006 | YES |

### Findings
1. **CORRECT**: Yugoslavia breakup comprehensively tracked:
   - F248-1920-1991: Yugoslavia (unified)
   - F248-1991-1992: Yugoslavia (rump, losing Slovenia/Croatia/BiH/Macedonia)
   - SCG-1992-2006: Serbia and Montenegro (FRY)
   - SER-2006-2008: Serbia (post-Montenegro, pre-Kosovo)
   - SRB-2008-2025: Serbia (post-Kosovo)
   - MNE-2006-2025: Montenegro
   - KOS-2008-2025: Kosovo

   CShapes validates: Serbia 2006-2008 area = 87,745 km2;
   Serbia 2008+ = 77,026 km2 (minus Kosovo's 10,719 km2).

2. **CORRECT**: Eritrea is tracked with start year 1882 (Italian colonial period)
   rather than 1993 (independence). This means the Eritrea polity covers both
   the colonial and independent periods as one continuous territorial entity.
   Ethiopia then starts a new period in 1993 (ETH-1993-2025).

3. **NOTE**: Hong Kong (HKG-1841-2025) and Macau (MAC-1800-2025) span their
   entire history as single polities. The 1997/1999 handovers didn't create
   new polity periods because there was no territorial change - only sovereignty.
   **Correct per WHEP methodology.**

4. **CORRECT**: North Macedonia (MKD-1991-2025) starts 1991 even though
   international recognition came in waves. CShapes uses 1991 as the
   independence date.

## Era 12: Recent Changes (2008-2025)

### Key Events
- **2011**: South Sudan independence (Jul 9)
- **2014**: Russia annexes Crimea from Ukraine (~27,000 km2)
- **2022-present**: Russia-Ukraine war (Donetsk, Luhansk, Zaporizhzhia, Kherson
  claimed by Russia but not internationally recognized)

### Database Status
| Event | Polities | Correct? |
|-------|----------|----------|
| South Sudan 2011 | SSD-2011-2025, SDN-2011-2025, SUD-1956-2011 | YES |
| Crimea 2014 | RUS-2014-2025, UKR-2014-2025, RUS-1991-2014, UKR-1991-2014 | YES |

### Findings
1. **CORRECT**: South Sudan independence perfectly tracked:
   - Sudan (1956-2011): SUD-1956-2011 (pre-split)
   - Sudan: SDN-2011-2025 (post-split)
   - South Sudan: SSD-2011-2025 (new)
   CShapes validates: Sudan 2011+ = 1,866,012 km2; South Sudan = 629,999 km2.

2. **CORRECT**: Crimea annexation handled through whep_fixes.csv:
   - Russia (1991-2014): RUS-1991-2014 (pre-Crimea)
   - Russia: RUS-2014-2025 (post-Crimea, +25,777 km2)
   - Ukraine (1991-2014): UKR-1991-2014 (pre-Crimea)
   - Ukraine: UKR-2014-2025 (post-Crimea, -25,777 km2)
   CShapes validates the area change.

3. **IMPORTANT - POTENTIAL UPDATE NEEDED**: The 2022 Russian invasion has resulted
   in Russian claims over Donetsk, Luhansk, Zaporizhzhia, and Kherson oblasts
   (~109,000 km2 total). However:
   - These annexations are NOT internationally recognized
   - The actual front line is fluid (Russia doesn't control all claimed territory)
   - CShapes 2.0 data ends at 2019, so it won't capture this
   - **Recommendation**: Do NOT create new polity periods for 2022 changes until
     the conflict is resolved and borders stabilize. The current tracking
     (Ukraine/Russia split at 2014 for Crimea) is sufficient.

4. **NOTE**: Other ongoing territorial disputes NOT tracked (and correctly so):
   - Western Sahara: Treated as ESH-1800-2025 (single entity)
   - Kashmir: Included in India/Pakistan respective territories
   - Nagorno-Karabakh: 2023 Azerbaijani takeover not reflected (small area)
   - Bougainville: Voted for independence 2019, not yet implemented
   - New Caledonia: Voted to remain French in 2018/2020/2021 referendums
