# Polities Research Report: COW Territorial Changes Cross-Reference

This report cross-references the 381 entries in the Correlates of War (COW)
Territorial Change Dataset (v6) against the current WHEP polity_codes.csv.

## Methodology
For each COW territorial transfer, we check:
1. Is the gaining polity represented in the database?
2. Is the losing polity represented?
3. Does the transfer year align with polity period boundaries?

## COW Transfers Mapped to WHEP Polities

### Italian Unification (1860)
| COW Entry | Year | Area (km2) | WHEP Mapping |
|-----------|------|-----------|--------------|
| Modena -> Italy | 1860 | 6,031 | DMO-1800-1860 -> ITA-1861-1919 |
| Parma -> Italy | 1860 | 6,221 | DPA-1800-1860 -> ITA-1861-1919 |
| Tuscany -> Italy | 1860 | 22,163 | TUS-1800-1860 -> ITA-1861-1919 |
| Two Sicilies -> Italy | 1860 | 11,950 | TWO-1800-1860 / KIN-1800-1860 -> ITA-1861-1919 |
| Papal States -> Italy | 1870 | 11,790 | PAP-1800-1870 -> ITA-1861-1919 |

### German Unification (1866-1871)
| COW Entry | Year | Area (km2) | WHEP Mapping |
|-----------|------|-----------|--------------|
| Frankfurt -> Germany | 1866 | 101 | Not tracked (below threshold) |
| Hanover -> Germany | 1866 | 38,474 | Not tracked (FT aggregates) |
| Hesse Electoral -> Germany | 1866 | 9,586 | Not tracked (FT aggregates) |
| Nassau -> Germany | 1866 | 4,709 | Not tracked (FT aggregates) |
| Multiple states -> Germany | 1867 | ~70,000 total | Not tracked (FT aggregates) |
| Baden, Bavaria, Württemberg -> Germany | 1871 | ~142,000 | Not tracked (FT aggregates) |

All absorbed into DEU-1800-1919 (Germany/Zollverein trade entity).

### Balkan Changes (1878-1913)
| COW Entry | Year | Area (km2) | WHEP Mapping |
|-----------|------|-----------|--------------|
| Bulgaria independence | 1908 | 96,345 | BGR-1878-1913 |
| Montenegro independence | 1878 | 4,584 | MNE-1878-1913 |
| Albania independence | 1912 | 28,748 | ALB-1913-2025 |
| Bosnia -> Austria-Hungary | 1878 | 42,520 | AUH-1800-1908 -> AUH-1908-1918 |

### WWI Aftermath (1918-1920)
| COW Entry | Year | Area (km2) | WHEP Mapping |
|-----------|------|-----------|--------------|
| Czechoslovakia from Austria | 1918 | 140,968 | F51-1918-1938 |
| Finland from Russia | 1917 | 325,534 | FIN-1917-1940 |
| Estonia from Russia | 1918 | 59,984 | EST-1918-1940 |
| Latvia from Russia | 1918 | 63,300 | LVA-1918-1940 |
| Lithuania from Russia | 1918 | 154,491 | LTU-1918-1920 |
| Ireland from UK | 1922 | 68,891 | IRL-1921-2025 |
| Hungary from Austria | 1919 | 91,075 | HUN-1918-1919 |
| Poland from Russia | 1918 | ~130,000 | POL-1918-1919 |

### WWII (1938-1945)
| COW Entry | Year | Area (km2) | WHEP Mapping |
|-----------|------|-----------|--------------|
| Austria -> Germany (Anschluss) | 1938 | 83,869 | DEU-1938-1945 |
| Sudetenland -> Germany (Munich) | 1938 | 28,674 | F51-1938-1945 |
| Finland -> Russia (Karelia) | 1940 | 40,000 | FIN-1940-2025 |
| Romania -> Russia (Bessarabia) | 1947 | 49,987 | ROU-1940-2025 |

### Decolonization Highlights
| COW Entry | Year | Area (km2) | WHEP Mapping |
|-----------|------|-----------|--------------|
| India independence | 1947 | 2,729,368 | IND-1947-1949 |
| Pakistan from India | 1971 | 13,361 | PAK-1971-2025 |
| Bangladesh from Pakistan | 1971 | 144,020 | BGD-1971-2025 |
| Ghana independence | 1957 | (British Togoland merged) | GHA-1957-2025 |
| Algeria independence | (not in COW) | - | DZA-1831-2025 |

### Post-Cold War
| COW Entry | Year | Area (km2) | WHEP Mapping |
|-----------|------|-----------|--------------|
| Croatia from Yugoslavia | 1991 | 56,410 | HRV-1992-2025 |
| Slovenia from Yugoslavia | 1991 | 20,256 | SVN-1992-2025 |
| Bosnia from Yugoslavia | 1992 | 51,107 | BIH-1992-2025 |
| Kosovo from Yugoslavia | 2008 | 10,887 | KOS-2008-2025 |
| South Sudan from Sudan | 2011 | 619,745 | SSD-2011-2025 |

## COW Entries NOT Represented in WHEP

The following COW territorial transfers involve entities not tracked as separate
WHEP polities. Most are sub-national transfers that are absorbed into larger polities:

### Small/Sub-threshold Transfers (< 10% of polity area)
- Bangladesh-India border enclaves swap (2015): 28.77 km2
- Botswana-Namibia Sedudu/Kasikili Island (1999): 5 km2
- Germany-Luxembourg border (1919): 3 km2
- Various small island transfers

### Administrative Territories (no independent trade data)
- Danzig Free City (1919-1939): 2,056 km2
- Saar Territory (1919-1935, 1947-1957): ~2,000 km2
- Panama Canal Zone (1903-1979): 1,678 km2
- Tangier International Zone (1924-1956): 363 km2
- Trieste Free Territory (1947-1954): 5,271 km2

### Pre-unification German States (covered by Zollverein)
- Anhalt-Dessau, Baden, Bavaria, Bremen, Brunswick, Frankfurt, Hamburg,
  Hanover, Hesse, Lippe, Mecklenburg, Nassau, Oldenburg, Saxony, Saxe-Weimar,
  Schwarzburg, Württemberg (all 1866-1871)

### Sub-national Colonial Divisions (covered by colonial entities)
- Northern Nigeria (1903) -> part of Nigeria colonial entity
- British Togoland (1919-1957) -> merged into Ghana
- British Cameroons (1919-1961) -> split between Nigeria and Cameroon

## Coverage Assessment

| Category | COW Entries | Tracked in WHEP | Coverage |
|----------|-------------|----------------|----------|
| Sovereign state formations | ~120 | ~115 | 96% |
| Major territorial transfers | ~80 | ~75 | 94% |
| Colonial acquisitions | ~100 | ~85 | 85% |
| Sub-threshold changes | ~80 | ~10 | 13% |
| **Total** | **~381** | **~285** | **75%** |

The 25% not tracked consists almost entirely of sub-threshold transfers and
administrative territories without independent trade data. This is by design.
