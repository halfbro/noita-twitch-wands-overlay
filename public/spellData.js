const spellData = {
    "BOMB": {
        "name": "Bomb",
        "description": "Summons a bomb that destroys ground very efficiently",
        "meta": {
            "action_type": 0,
            "action_max_uses": 3,
            "fire_rate_wait": 100,
            "speed_multiplier": 1,
            "action_mana_drain": 25
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAjElEQVR4nGNgGDCQ7mRZl+hLgmoIoy7RlyhtcA2YgBGNr6FtKSmtBGE/f3rvxtXjaAqYcanW1LFQVNH9/OnDm9dPsGuAqH7+9B4vn6CmjgVEEFMPE5wFUQ23AVkcmcvEgA1cv3ICjQEHLGh8uCWYStFteP70HlYGnAsBKMGqoY0S/BBfoYUsyfFAewAA/MJCoxWYACsAAAAASUVORK5CYII="
    },
    "LIGHT_BULLET": {
        "name": "Spark bolt",
        "description": "A weak but enchanting sparkling projectile",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "spread_degrees": -1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAQUlEQVR4nGNgGAXkgrWFn/BwsSt9tuzns2U/IYyfr/9C2NjVrS38BJGGqIOrRteDLARRB9EJEcTnKmTNhNWNCAAAKWA/S/mQ4KkAAAAASUVORK5CYII="
    },
    "LIGHT_BULLET_TRIGGER": {
        "name": "Spark bolt with trigger",
        "description": "A spark bolt that casts another spell upon collision",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAi0lEQVR4nGNgoAR8nixOUA0ThPp+v5qBgYHFwALOxgUY4azvh/0ZZHQYnlzhtN1IQMPnyeIsBhYMMjpQsSdX/lw4wZv7kjo2wFTfr4bqIeQHFEBMKFEJrC38hIeLXemzZT+fLfsJYfx8/RfCxq5ubeEniDREHVw1uh5kIYg6iE6IID5XIWsmrI76AAD6KWBDF25sYQAAAABJRU5ErkJggg=="
    },
    "LIGHT_BULLET_TRIGGER_2": {
        "name": "Spark bolt with double trigger",
        "description": "A spark bolt that casts two new spells upon collision",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 4,
            "speed_multiplier": 1,
            "action_mana_drain": 15
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAoUlEQVQ4jWNgoDb4PFn8PynyTDDG9/vV/xkYGBhYDCxQ+ITkGVEUHfb/zyCjw8Dw5AoDp+1GFDlc8owwZ7EYWDAwyOggVD+5wvDnwgkG3tyXjPjkKXYBhh+/H/b/jy8McMnDAamxQFvg2HcZwzZsYng1lp76/7/01P//MPbP13/hfLyaHPsuwxXCNCFrxmoIugRME8wwmBxR3kA3kCRNdAUA1/KmWJGbUn8AAAAASUVORK5CYII="
    },
    "LIGHT_BULLET_TIMER": {
        "name": "Spark bolt with timer",
        "description": "A spark bolt that casts another spell after a timer runs out",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAwklEQVR4nGNgIBEwQijt4OkCEiYQ9ocXZ66uzcSlgRmuuiNHR4PtWJCf3ZFrLHxyRq+vb8Vpi3X26cPXv79+/fr169fS5umHr3+3zj6NSzEThHpyYQMDA4OBT82FLS0QNgENMgYBDAwMF7a0wNn4/MAnZ3TkGouBjsyFY9t+8elVTLny4cUZXH4gOZSoBNYWfsLDxa702bKfz5b9hDB+vv4LYWNXt7bwE0Qaog6uGl0PshBEHUQnRBCfq5A1E1ZHfQAAXeB6Zb3jgvEAAAAASUVORK5CYII="
    },
    "BULLET": {
        "name": "Magic arrow",
        "description": "A handy magical arrow",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 4,
            "speed_multiplier": 1,
            "spread_degrees": 2,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAK0lEQVR4nGNgGAXUAPnLuamhJ3859/H3yf///4eQmOD4+2SauYocPww3AAAk4SPZa4mzvAAAAABJRU5ErkJggg=="
    },
    "BULLET_TRIGGER": {
        "name": "Magic arrow with trigger",
        "description": "A magical arrow that casts another spell upon collision",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 4,
            "speed_multiplier": 1,
            "spread_degrees": 2,
            "action_mana_drain": 35
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAdUlEQVR4nGNgoAR8nixOUA0ThPp+v5qBgYHFwALOxgUY4azvh/0ZZHQYnlzhtN1IQMPnyeIsBhYMMjpQsSdX/lw4wZv7kjo2wFTfr4bqIeQHFEBMKNEA5C/npoae/OXcx98n////H0JiguPvk2nmKnL8QBEAALkQRNFvH6gTAAAAAElFTkSuQmCC"
    },
    "BULLET_TIMER": {
        "name": "Magic arrow with timer",
        "description": "A magical arrow that casts another spell after a timer runs out",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 4,
            "speed_multiplier": 1,
            "spread_degrees": 2,
            "action_mana_drain": 35
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAArElEQVR4nGNgIBEwQijt4OkCEiYQ9ocXZ66uzcSlgRmuuiNHR4PtWJCf3ZFrLHxyRq+vb8Vpi3X26cPXv79+/fr169fS5umHr3+3zj6NSzEThHpyYQMDA4OBT82FLS0QNgENMgYBDAwMF7a0wNn4/MAnZ3TkGouBjsyFY9t+8elVTLny4cUZXH4gOZRoAPKXc1NDT/5y7uPvk////w8hMcHx98k0cxU5fqAIAAAcx17zpKEU7wAAAABJRU5ErkJggg=="
    },
    "HEAVY_BULLET": {
        "name": "Magic bolt",
        "description": "A powerful magical bolt",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 7,
            "speed_multiplier": 1,
            "spread_degrees": 5,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAYUlEQVR4nGNgGAWEASNW0f7D6ioyKhoKBgwMDCoMLXcYahgYGFQZW1E0QBTdeXIHolSFoQXNlDsMNaqMrYwQpXBRuMEIEZgNNx5c8FXciu4kiGYVGRUIF6IZopR0/45cAACIpRsF5P/qWAAAAABJRU5ErkJggg=="
    },
    "HEAVY_BULLET_TRIGGER": {
        "name": "Magic bolt with trigger",
        "description": "A powerful magical bolt that casts another spell upon collision",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 7,
            "speed_multiplier": 1,
            "spread_degrees": 5,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAq0lEQVR4nGNgoAR8nixOUA0ThPp+v5qBgYHFwALOxgUY4azvh/0ZZHQYnlzhtN1IQMPnyeIsBhYMMjpQsSdX/lw4wZv7kjo2wFTfr4bqIeQHFEBMKNEeMGIV7T+sriKjoqFgwMDAoMLQcoehhoGBQZWxFUUDRNGdJ3cgSlUYWtBMucNQo8rYyghRCheFG4wQgdlw48EFX8Wt6E6CaFaRUYFwIZohSkn3L3kAAALbO/30Q4QxAAAAAElFTkSuQmCC"
    },
    "HEAVY_BULLET_TIMER": {
        "name": "Magic bolt with timer",
        "description": "A powerful magical bolt that casts another spell after a timer runs out",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 7,
            "speed_multiplier": 1,
            "spread_degrees": 5,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAA4klEQVR4nGNgIBEwQijt4OkCEiYQ9ocXZ66uzcSlgRmuuiNHR4PtWJCf3ZFrLHxyRq+vb8Vpi3X26cPXv79+/fr169fS5umHr3+3zj6NSzEThHpyYQMDA4OBT82FLS0QNgENMgYBDAwMF7a0wNn4/MAnZ3TkGouBjsyFY9t+8elVTLny4cUZXH4gOZRoDxixivYfVleRUdFQMGBgYFBhaLnDUMPAwKDK2IqiAaLozpM7EKUqDC1optxhqFFlbGWEKIWLwg1GiMBsuPHggq/iVnQnQTSryKhAuBDNEKWk+5c8AABmg1YfF74n6QAAAABJRU5ErkJggg=="
    },
    "AIR_BULLET": {
        "name": "Burst of air",
        "description": "A brittle burst of air capable of greatly pushing objects",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "spread_degrees": -2,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAYElEQVR4nLWRXQrAMAiDdexo3jq9W/YgiKw/rozmKYV8UanICQEA4P460vopTZKkMzXMpBd8991m5r61JiLxHCwwK84TCiyA4oa+PhhdMG7yDarT/Hhakct+4xO39bf7ASmgpJyxEBsjAAAAAElFTkSuQmCC"
    },
    "SLOW_BULLET": {
        "name": "Energy orb",
        "description": "A slow but powerful orb of energy",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 6,
            "speed_multiplier": 1,
            "spread_degrees": 3.6,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAA30lEQVR4nGNgoBD8//8fU3DOge/ofHQh3ICFgYFh1oEfaQ4cED6301Rtuyi49NVDy77uy4awzRren2oQRNHN7TTVrOH9nAPf/////////zkHvps1vOd2mordLmTVePQwwllmDe/hDoODZHuOuQd/zDrwA+4YJvxenHvwB5oIioZkew44A4KQBSGABc08NGlMS5jhrA9Mco//azIxMhgpsMw9+MNIgYWBgeH8wz+zDvy4emjZ7/vb0DX8vr8NrgeiFK4aHhUMyKEEAXgijgQAiRYIGxpKZg3v8evBDF9iAQCfUH+gVxKP3wAAAABJRU5ErkJggg=="
    },
    "SLOW_BULLET_TRIGGER": {
        "name": "Energy orb with a trigger",
        "description": "A slow but powerful orb of energy that casts another spell upon collision",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 25,
            "speed_multiplier": 1,
            "spread_degrees": 10,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAzUlEQVR4nGNgoAR8nixOUA0ThPp+v5qBgYHFwALORgZzDnyHsxnhrO+H/RlkdBieXOG03UjAhs+TxaGqGRgYZHS+H/bH6jazhvfYbbDfuwAueKpBELtFEHd/P+xv1vD++/3q////////f86B7xBTcYLPk8XnHPgOUYpLD8JJZg3v0xw40IxItueYe/DHrAM/4G5jwmcjA8Pcgz/QRFA0JNtzwBkQhCwIASxo5qFJY7UEAcwa3pPgabgeZK8jexdFg1nDe7gEspE4I454AAA3Unc9ZBgluwAAAABJRU5ErkJggg=="
    },
    "SLOW_BULLET_TIMER": {
        "name": "Energy orb with a timer",
        "description": "A slow but powerful orb of energy that casts another spell after a timer runs out",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 6,
            "speed_multiplier": 1,
            "spread_degrees": 3.6,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAA90lEQVR4nGNgIBEwQijt4OkCEiYQ9ocXZ66uzYSrMGt4f6pBEEWTdvB06+zTh69/X758+eHr362zT2sHT8dnC0T169evX79+LW2eDtHDwMAw58B3TMVMEOrJhQ0MDAwGPjUXtrRA2AwMDCkOnHB1cw58////P0KDjEEAAwPDhS0tcLZZw3uzhvfIZs89+AOfH/7//z/nwHc0PThDqTA3IdmeA2LqrAM/kEOJEc4ya3if5sCBZl6yPQeaHias4YbF3TCAogHiDAgDgpAFIYAFzTw0aayWIIBZw3tIeENIrAHFiKkH2etoQQTVMOfAd+RIRTYSPdmRAQDyi4687ul/kAAAAABJRU5ErkJggg=="
    },
    "BLACK_HOLE": {
        "name": "Black hole",
        "description": "A slow orb of void that eats through all obstacles",
        "meta": {
            "action_type": 0,
            "action_max_uses": 3,
            "fire_rate_wait": 80,
            "speed_multiplier": 1,
            "action_mana_drain": 180
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAwklEQVQ4jWNgGDQgpHbGLGzshN3f/yfs/v6fZM0wjarSpv9VpU3/E2VQwITd22GaYRrRMbohTMiaNxS4eibs/v7/aIIdAwMDA0OUTz6GJUcT7BiQDWHCUAEFUT75DA0zorEaggFC5l88gs3p9elLMLwws/oYildwuoCBgYFh2ZaJGGI9C1BdxMTAwMCwJlHfJmDC7u0LXDkZrRccgnsB3UswYL3gEMMCV05GvC5Atp3o8MAXjbDwwJoWqJKQKErKAwoAthidjANSgYoAAAAASUVORK5CYII="
    },
    "BLACK_HOLE_DEATH_TRIGGER": {
        "name": "Black Hole with Death Trigger",
        "description": "A slow orb of void that eats through all obstacles and casts another spell as it expires",
        "meta": {
            "action_type": 0,
            "action_max_uses": 3,
            "fire_rate_wait": 90,
            "speed_multiplier": 1,
            "action_mana_drain": 200
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABU0lEQVQ4jaWSrW/CUBTFD3UkiMklna1CDLFmogsOV0FSFDOdggWBJdmWIZATLCGBzaxiG4YlEySIVvExQ0WXdIE/YKJJEXWVd6pv/YIt4SQvuSfvvd+9774L7KlMEOSVPh0cnrANzzHx9XaZSb0VV17pk9RY0mzl03A4pNnKJ6mxpLzSp38Bgsuu65LrusSf1hhE1X1SdX8riAuCb+sdAFCQr2GNO8wv1CIWahHbQAxwVCgDAKxxJ+IDhUEJgOeYaPVsrDc5GIaB9SaHVs+G55iJkuOQnb9gj+po11/wOr6P9kybQitlf3+o8vQ5BwBV90ngRbZua88RL/AiPVx9kMCLrB8cdiieGQDutGbEcwAwujg+K3f1iVbKZiRtCgCoytGDYR9+wtYKwtmrchPtwXkCmlC5q0+AZC/C/UgdqsrN4DEeB4MTAP6ayFRIGLS79j30A1qWzP3uVZ7jAAAAAElFTkSuQmCC"
    },
    "BLACK_HOLE_BIG": {
        "name": "Giga black hole",
        "description": "A growing orb of negative energy that destroys everything in its reach",
        "meta": {
            "action_type": 1,
            "action_max_uses": 6,
            "fire_rate_wait": 80,
            "speed_multiplier": 1,
            "action_mana_drain": 240
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAA2UlEQVR4nJ1SsQ2DMBA8W2mYIh1LIKVDomAABxoXFKnCClmBVCkoXBEGoECii8QE6ZgjEm2KR8Y2cpBy3b3u/P++Z7Ahh9mpqDgwKXOl/TS2hS5GokYSmraDVo/yhA2On3cjCxKQh5nqLC2brtralm7qpeKAa56l5e2RZ2npMywjyWFGPwFougoX/OiAfpLDzAGMbUEPm2qz1fPekgwA863rW4Pvq2y4Buq+Y4hErfn5KrzziBoAV3FAWfqwbp+EVg4+tRPOckvmXzl5E6WY8cfxrQbLZsA57y993l945pTbtQAAAABJRU5ErkJggg=="
    },
    "TENTACLE_PORTAL": {
        "name": "Eldritch portal",
        "description": "Summons a one-way portal to a sinister realm",
        "meta": {
            "action_type": 0,
            "action_max_uses": 5,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 140
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAB0klEQVQ4jbWTP0sjQRjGn5iD2MjGgGRxGVeLC7d6MZuAoIH7ArnmGrt0dp7gFxCLcF8gYGOxfSDdQUQbQQKb2JhoihWtxjljErz8OZurdq8I77C7Z6lvM8PMPO/8nocZ4L3q6sfddaVY88Jr4XMz4YVywWpVijXv7vZ+Pb+ZR/37jVcuWC0AqNt19+FoFGj6gSb+De4IPE4EuCOgGwyPE2GWC1YrmVDN8IVRQjs7P3sa/R6p7gugGwzuy/TA6q6OyeVfuDOuqikM8QUFy0h9O73/eSwt5A5Smf2TnWx/2GvTzVuHaQBAtzoGAGgKg24wcEfATyIzIERNYVKoGwyL23HoBsPWYVpa8wcqM8itbJgk6FbHWNyOB7w2Sp0Aid2012UGlWLNS2c/gzsCvy4GmDz/QXQQw9zaLABgbm0W0UEMq7s6ooMYuCPw1foSkQSpTx9vqKOmTHHJO43cEeClqYX+sNcOZJA7SGX8eI1SB9wR6FbH4I6Qdki8f7KT/S/E/rDXppQpKBI2Sh3oBoOmMCQTqlkuWC0KMeIP6uFo5NlNG/1hr51MqGZ+My8bEh3V0t58JEDgJ6E3QS+R8O2mjaW9+QiJX63wh6kUa95rn+jN6h/i4ewJNXtrCgAAAABJRU5ErkJggg=="
    },
    "SPITTER": {
        "name": "Spitter bolt",
        "description": "A short-lived magical bolt",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -1,
            "speed_multiplier": 1,
            "spread_degrees": 6,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAsUlEQVQ4jb2RsQ3DIBBFP1bkjl08ijtvQOnMEkoWsOmYw27YxZVpSBHOIgjHF0XKr7iDe/r8A/6tbTBxG0yk+pZdPACMqdTSqnttuO271xkmSqtEkw+3fYf0YEw9vq19WiJpn5Y3m6wvcCWtEnlNAB2cp/8jOA8AmgM8aLUQOcGKslECKXVyVYNUh8+CZW2nHP60neaSdqEzgA7OI8zr0QjzWt3OzyGeAnIIuWJt4Fs9AS4umq60O0UAAAAAAElFTkSuQmCC"
    },
    "SPITTER_TIMER": {
        "name": "Spitter bolt with timer",
        "description": "A short-lived magical bolt that casts another spell after a timer runs out",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -1,
            "speed_multiplier": 1,
            "spread_degrees": 6,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABN0lEQVQ4jaWRsWrCUBSGv6QlkwGHDj5CQYcMbnZ07CA4pGOnQFFsR4eOHVwrgpAXUIdCho5uLsU4ZLDo0LGDoEMgBTHL7WIuaWoaiz9cOPfc83+ccw+cKCUKivW+yBfK8sFfzXh/uVOShsC0BYA+shQANW7uNEs0rz7oNEvkC2WK9b5ImrWagVYzJEgFiMyXF19Uq1Vubh8kJEtqFHx6DgDG9SPe65O8x6WPLCV0PELHkyMAUGm4YrLYivV6Lc9ksRWVhit+UQ514K9mtHtzlpsc4/GY5SZHuzfHX80yR/hzC2/nZ12gtU919ZF1nwpIKjDtZ6Cl1QwAQsdLhRw0B6YtdoOpiLQbTEVg2mIPzgT8MCch8Vo1DXKs0gDd0PEIh65MhENX/kO88ORPTAXEIVFXR23gv/oG1KW0queXkzgAAAAASUVORK5CYII="
    },
    "SPITTER_TIER_2": {
        "name": "Large spitter bolt",
        "description": "A more powerful version of Spitter Bolt",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -2,
            "speed_multiplier": 1,
            "spread_degrees": 7.5,
            "action_mana_drain": 25
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA9ElEQVQ4jWNgoDcIepZ7JOhZ7hEYnwXGsD4Z2iguK+HMwMDA8PLxi71HzVfXY9McKylszcDAwMDwLPfIOqnJNizImmGSiyFiDNgMwems9f8b/sPA+v8N/5GdSZQXiAXrpCbbIPNZGBggfl7MwMDAINkI8cLzt0dfPn6xlxgDGWEMbIFITMAyogugGwgPWKir0A1hJqQ5gKGeQYPBgYGd95TcXSZmFqEwefnHc64dwOsv9FjBFztMeE0iAmA14OXjF3sXP397dANDI1xsA0Mj1tjBGgaP51w7IBQmL3+XiZmFnfeU3A2GgzgDEWcsMDAQlz8oBgCbo6kf7eoYigAAAABJRU5ErkJggg=="
    },
    "SPITTER_TIER_2_TIMER": {
        "name": "Large spitter bolt with timer",
        "description": "A more powerful version of Spitter Bolt that casts another spell after a timer runs out",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -2,
            "speed_multiplier": 1,
            "spread_degrees": 7.5,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABbklEQVQ4jaWRP0gCYRjGH0/BRUE08BT0VkGHBqHhHAIdG0JJWoTWUKEpHAI1GtwVXNxujNShUcPQG1IHB1OHnC68Ez1wCBq/prvkutPAH3zw/Xse3vd5gQMxKZtgokocdFh92EhDvD9dm7SC+CLbA4C6txwBAGpbXMqEkIl8oJQJwUGHEUxUiVac8rjYlMfFKkYUACjiwNEXYrEYLq9uVJN9UMrmc9QEAByf3WH0/KCet6l7yxFOlHlOlHmlBQAAmx6Q7vSbrFYrdXWn34RND8gfF70KNtIQucoYs7UNrVYLs7UNucoYG2m4t4WdU3Dcvty7fXQUAJaC1OZPHvOGBlrYt4ui20dHUx4XCwCcKPN6JuZ94nPkEcAprPa+f06ZLc4kwwi1SWdnX/FFttcgBaKlQQpEmb8CZWTyX3QNloLU5kSZb6Ko3jVRVHPY/qubgVCbdJxJhplTZovV3vfP8GoYouEUgN8wlar0xngwP886vZ2vCUxdAAAAAElFTkSuQmCC"
    },
    "SPITTER_TIER_3": {
        "name": "Giant spitter bolt",
        "description": "The most powerful version of Spitter Bolt",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -4,
            "speed_multiplier": 1,
            "spread_degrees": 9,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA5klEQVQ4jWNgoDfY2fN+/86e9/sxJDrib2fDJDvib2fj0vzi4P//Lw7+/w8zhAWm2VBXJETfVMAByUCGioWqU4l21ouD/+EA2QZCXmAhygYk4F4i6IjMZ2FgYGA4f/nNGgYGBgZ9BogXLp7+cAAmRggwwhiwcIAZWLFQdSo2MaKdCosV5FDHFjvMuDTDYkXcjoGBR56BgecPhwLjHyYuHckUnj0XJ5/Gazt6rOCLHSai/YQDYI1G5FgRt4OIvTyEPXawhsGei5NP60im8DD+YeLi+cOh8PUhQjN6TDBiMwAGKIpGYgEAWuW4CKsYPakAAAAASUVORK5CYII="
    },
    "SPITTER_TIER_3_TIMER": {
        "name": "Giant spitter bolt with timer",
        "description": "The most powerful version of Spitter Bolt that casts another spell after a timer runs out",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -4,
            "speed_multiplier": 1,
            "spread_degrees": 9,
            "action_mana_drain": 45
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABaUlEQVQ4jaWTP0gCURzHv3c1iLxBQUinXILAExNsEiLhwKVNh5awwSXOocHBobHhhjYN5yYXg4aW4CD/jE/QQ0PBKWi48KAbCpeD1xDPrst/0QcevPd+7/v7ywP+icA3kUyV+YKJmcEyOni6PRPcgoert0cASBf9KTjFSYWy9nDKarUaaw+nLKlQFslUmVtsNBkzmoxxRyIA+IIJqAUJu4F3yLKM49NzqAUJzowWIfLNS+8OALB3dIHe/eXs7CRd9Kd0ajV0ajV+lMDTn0wms8XLWCsDy+igVBlgZBJomoaRSVCqDGAZnZUlLJ3CCZEL8WggCwDdvlkv3excr/TIUXNjxd11NTdW3O82Fonj0UA2tu873DoAyDZAbE9YsEWvFMoTTS/TpdF5ZDfO+XPERU7WZXPeZbdv1gEghq8SAOC1BejUanAbZ24PNL1MpVCeCLboJbYn/PH8LXZP4tdnccKbybP60xjX5RO+mdERBZQwZQAAAABJRU5ErkJggg=="
    },
    "BUBBLESHOT": {
        "name": "Bubble spark",
        "description": "A bouncy, inaccurate spell",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -5,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAtElEQVR4nGNgIBEwovE5feeIGIcxMDC8Obvq++YUAro5feck7f+27t6vdfd+Je3/xuk7h4AG2YZP6+79+v///////9fd+yXb8AlTDRPxrocAFmTOm7OrtthHQdhbHv55c3YVpgbKPE0MQLeBWAs5fefINnySbfiEHJQ4QxmXBNZQZmJgYBAxDvORZwlUZA1UZPWRZ4G4ARdgwSOHNZRZcEkwMDB835yynIFhN6qnGQmEBuUAAJU5gBVxKUWhAAAAAElFTkSuQmCC"
    },
    "BUBBLESHOT_TRIGGER": {
        "name": "Bubble spark with trigger",
        "description": "A bouncy, inaccurate spell that casts another spell upon collision",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -5,
            "speed_multiplier": 1,
            "action_mana_drain": 16
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABJUlEQVQ4jbWSsUrDUBiFv5Z0yOBWrNCG0r1bH8AXaIhdOjiGrj5Bliw+QR2zCxEC0oKgQ4a4ZnNOEFyETqn0Sij8DrWl2Ggi4rfdy7n/PfecC3+k9nVjOW3J8f0lzcEYgEXso2aTA90BKnUEQEWW2OFKVOpIkORihyvRTU8q2VGRJSp1REWWbAmSXAw3+3ZAfWtbRZbQ6W92O33eH894uzopvVQDOLp4rcEtKmIz5OWJu/YNtGH+vGYR+xXs72Wgm56o1BHDzaq/f8ty2vrVgfJ6PtFNT36sVjc9Mdys0LZuemKHKwmSvLjaMoHhZhIkeWG1dYDmYMywqzHqNRj1Ggy72u4nlqFVES1in/np+W69X61WJgBQs0ntGuShIMRdkqUp/xcf7MDKvFnmKWYAAAAASUVORK5CYII="
    },
    "DISC_BULLET": {
        "name": "Disc projectile",
        "description": "Summons a sharp disc projectile",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "spread_degrees": 2,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA/0lEQVQ4jc2RP2oCQRTGfyteYRcFZSHY2GwRrIwHEGxyAatp7L2LTLPVXMBmYA8QrUTINjZpRCFhc4hJ40yeYVckRcgHUzzmvff9efDvoJQqALSxK4DycHL+/XqpNtZpY8MSuax1YV3Wsa6LvdPGhubtrkTWANEt1qa/TtzlefoYSQXe9+anRIDjueJ4rkL98fmO72srpZZ5nk+1savFfPZUHk5uuyuvhqu3l0s1Ie0lVwpbTTLvRe2CxXwWskl7CclgQjL4ZgcYjzKyYT9qDHFd7J33eoskKBCn3AA8pHHtYCfuMh5lIeh2Tc+rZJGhSmsejRYk5FmzYf+umb/DF0J3gVs85ce1AAAAAElFTkSuQmCC"
    },
    "DISC_BULLET_BIG": {
        "name": "Giga disc projectile",
        "description": "Summons a large, serrated disc with a curious flight pattern",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "spread_degrees": 3.4,
            "action_mana_drain": 38
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA50lEQVQ4jaVSsQ2DMBA8I4ZwnZRugtwlZepImYQNoACJAShoWAKJmjLpEGkok/q3IJUdY5sQhass6/7+7v+BjWBrhKbtJgDohxF5Gjv8YKlAvfthXCx2HKgCIgLnXP8TEeqqYEr8ejmzrwJrkJHQIgEAJFnpKC+BiGa8UCkmWTkRkUPmnM8imdG0AwU79+tJuN8eTpE56EBZl5GYkWwxE/0wahHtwM5PRDieDtjt+Vex0JfZdGDbr6uCAZ/B667/rNCJICMBGQnkacx8G/FhFsFUVtbNjk3bTfasvIeTZOWktvLLcW3CG5B1fichODZvAAAAAElFTkSuQmCC"
    },
    "DISC_BULLET_BIGGER": {
        "name": "Summon Omega Sawblade",
        "description": "That's a lot of sawblade",
        "meta": {
            "action_type": 0,
            "damage_projectile_add": 0.2,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "spread_degrees": 6.4,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABz0lEQVQ4jY2TT2sTURTFfy8dIQSLtWlhFg2hgosJCq7VFkJCB3ECgSz8FgUXLu3C7rIKfpJAZiMkBNLYhSAtpM0sAi1xoj6IYDAbF2meC/Me4yQFz+r9uee+e+49T7AC3WpNZTtNkfF9AOqNltJ35VJBRGNFjEu3WlNfwm/sXffFcL+oAD78TgBwfHQoQs9DJwawouTz12/UnU+n9FKbnOzmVLAgAkgpqTdaahj0CMFUZyrQL9vTCe0Hj1YpAyB/dcHm/XukB4HI+D4Jre+H85hge8cErYIzHgHQnwm0PFFvtNTns0uklNi2jTMemURxsj2dINc3aN9Y5NdmpA5cRLdaU7pJmuyMR7RvLGzbNvrza7N/Eh8fHQoAEXoew/2iSRLF6cdzAJ4+e7JUya/KK8qlgkhkfJ+737/eqjuOYHsHub5h9gIg9Ly/Y4uUqHsSX0fhJufLTfwf6J7kLEWiXCoINzm/laxHF8XLFwX2rvsCFk7Mdpqi8tBR/ZlYOcI4toLews6+SABkfJ/0IBA5S+Em50gpTbA9nZC/ujBnbnJOttM0Dl76TABv371XAJWfIQDpQSBOdnMqdeAu/UYrTtbW3gp6xu8A3WqR5zEywB/w0td10zyVhwAAAABJRU5ErkJggg=="
    },
    "BOUNCY_ORB": {
        "name": "Energy sphere",
        "description": "A fast, arcing projectile",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA4UlEQVQ4ja2RrRKCQBSFP80sNqszZIIBmy/EM2CE5APwLI6VBoFAZoZKEtaOQdZZ1sW/8aS9u+d8c/de+KfCrM//CrTpeBkGvV686CYGorFM0r17sPmsAPUVzxcBQF3JAiDdu7u3gDDrc0eIYL2B7eoKQNk5tA1cpSxMyNLSNnoY7uf1ZuqxAoDI80Wgh3XI+KVIvzcBX8sEJHUli7Jznoxl56hhJrMAtaq2uQf0cNtMPUqPLYRZH6vHb9a4VGHPF5Ga8Gg81ZWkriTASQ+bm5i/tCjM+vg8yOFT/yzk57CpGxWnblNGdhdCAAAAAElFTkSuQmCC"
    },
    "BOUNCY_ORB_TIMER": {
        "name": "Energy sphere with timer",
        "description": "A fast, arcing projectile that casts another spell after a timer runs out",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABTklEQVQ4jaWSoW/CQBTGv6LbglyyKSyICsgE9TMTS7DzS4NegkAWhZkguT8BsyUTmGU46jhR0QUUCckEoYjSa4J8E+wa2h7byD517/p9v3uvd8A/pclFrc2octFIP0Rrjo8XR1Om8qq1GbU6M5rO9zQajWg631OrM6Nam9GfADIchiGFYUiX1w8pJO8dRJTZK8nFp/8KALBue/DHblo7XkyOF7vS91jRNCXgyroDAPhjN1NX6wYA3DhePFN1XwIOP6w7DLDY6phMJlhsdXSHAaI1h1VOUK0bDd0wGirIj7fw9Hyf1v5OxzIQHMAbs81eASDnrdYNWOVE1a2EgNlmmispnWcoD+gvA8H9nX7qdA6gfxIgZ9usDoHj8GaV9RQA8q6ZbTYTIbjsRJ6cCMGZbTaPvTgODyLKPBjHi93vR1TYfydBSkhh8BM6x/urvgCzrsTAES4augAAAABJRU5ErkJggg=="
    },
    "RUBBER_BALL": {
        "name": "Bouncing burst",
        "description": "A very bouncy projectile",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -2,
            "speed_multiplier": 1,
            "spread_degrees": -1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAfklEQVQ4jWNgGAWMuCRM29T/SxgKMzAwMDC8OP+W4XTVTaxqmZA1YNPMwMDAIGEojCGP4gLTNvX/MBuQNXuYyTIwMDAw7Dj1GMMlMD1MMBuQbUPWjMzGpg7uBXIBE8xpMABjw5yN7gV0dfCQxRUOyBrQ/Y9iADogNhpHAQMDAFDASUmhkPUuAAAAAElFTkSuQmCC"
    },
    "ARROW": {
        "name": "Arrow",
        "description": "Summons an arrow",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "spread_degrees": -20,
            "action_mana_drain": 15
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAARklEQVR4nGNgGAXEgalGRidiYqimZ/H8vnt3r8O5jHBWqr8esjpTLYnT116YakkwMDBwqnkwMDBY23kqKWuSbAO5fhhZAACBchiqTvpNEwAAAABJRU5ErkJggg=="
    },
    "POLLEN": {
        "name": "Pollen",
        "description": "A small, floating projectile that homes towards nearby creatures",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 2,
            "speed_multiplier": 1,
            "spread_degrees": 20,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABGklEQVQ4jb2SP2rDMBjFXzsmEDoEH8BejCk9QYdqiLPmGh4y9gSlBygdBA2UnqAEOkUZRCCDM2VzvUiQJZTaQ9AJOlQ2iuSqgULfIvH4+H1/gT/qrMtknI4BxIZVpiRbnARgnE4AXA8GF8PGU+pQA1inJJt7ATrzKAqT0A4UspAAlnYl51ZcbGYGEDQf7cdWvAMwFfR7wdCEdMkH+LTekwClHpgj7ZdegB7QWshCVvVeAUBV75Ue4K5rlU4LelVLpQ5bnXmL7+k/+lppVnikj+rdKZlxOhW71SvjdNpWwDgdX13ePNiQPN84iaIwIf1ekERhQlpASrLF7OkZP52rKSELnucbCFnw32Jxd3/rtAAA87eX1vfdwf/oC2KebbJR8XmXAAAAAElFTkSuQmCC"
    },
    "LANCE": {
        "name": "Glowing lance",
        "description": "A magical lance that cuts through soft materials",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "spread_degrees": -20,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAOUlEQVR4nGNgGAUUAIu2N7OufZh17cP///8t2t6QoO3///////+fde0DdsMgJFaAooc0G6jph+EJAGXrUDr+KU7XAAAAAElFTkSuQmCC"
    },
    "ROCKET": {
        "name": "Magic missile",
        "description": "A fiery, explosive projectile",
        "meta": {
            "action_type": 0,
            "action_max_uses": 10,
            "fire_rate_wait": 60,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAfUlEQVR4nGNgGArgrYPoWwdRotTB2V+7Xb52u3w/H0+UNrjSr90u35+3////H1kBI1ZtX7tdmFykGSQ0/i3ZyxTjzCFRwcDAwMjIiJCGuwdu/Pfn7XAbICSaVQhtED2YqiEayHISmtLv5+O/P2/H5WlE4ODSRhgQG3GDBgAAYYhrmVipdvgAAAAASUVORK5CYII="
    },
    "ROCKET_TIER_2": {
        "name": "Large magic missile",
        "description": "A more powerful version of Magic missile",
        "meta": {
            "action_type": 0,
            "action_max_uses": 8,
            "fire_rate_wait": 90,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAhElEQVQ4jcWS2wnAIAxFHUcK/mUJ58gKzuJKLtA9HOH2S9BofNBC75+Sc0I0xvwRvi80FykzlNohDACdRMYGDxv8sEiFJeAiwUVadxuJatBFQsoMAGeiIkmZUUs6kYvUzFt3l7AqkaIi0eCtcbZHmIHlvHzE0Z9L0VFmi/Q6qvh4477IA9gttjeQwL1nAAAAAElFTkSuQmCC"
    },
    "ROCKET_TIER_3": {
        "name": "Giant magic missile",
        "description": "The most powerful version of Magic missile",
        "meta": {
            "action_type": 0,
            "action_max_uses": 6,
            "fire_rate_wait": 120,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAvklEQVQ4ja1Tuw3CMBS8NwISJRIbsACiYY5swQJeIF1GYAP3FEEskBLR0KMwwlGA5Zf4xcFSrrLsd/fufQwsBe9I62yhb0fv3pF9S84RA5n8xk86AIC6IuvKFkzImjQW9Y7smnlniZAmhhLJVEhyQt6R2zWw2QG3C7A/AqvDjygig0BtX2cPzdXNC0gceEfeH8DpLBKyP18x8/saHQQXy5RgEbsmzjrXxAGsMWqhIuQWybwvXpYp5P7DP3+lGB8YvMLMx8RSiAAAAABJRU5ErkJggg=="
    },
    "GRENADE": {
        "name": "Firebolt",
        "description": "A bouncy, explosive bolt",
        "meta": {
            "action_type": 0,
            "action_max_uses": 25,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAc0lEQVR4nGNgGK7grYPo124XrFIsWEW5+r0YJDS+MjBwl+4hYPbXbpfvz9v/////////78/bSVAN1XM+/q2DKLIaJmTOj60XGV7cQDFDQoPDW5+APchOwvQ6Fk//W7L3RwwDw4sbnIYLCZiN7Bk0pw8rAAAlWkFNo2GmWQAAAABJRU5ErkJggg=="
    },
    "GRENADE_TRIGGER": {
        "name": "Firebolt with trigger",
        "description": "A bouncy, explosive bolt that that casts another spell upon collision",
        "meta": {
            "action_type": 0,
            "action_max_uses": 25,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAt0lEQVR4nGNgoAR8nixOUA0ThPp+v5qBgYHFwALOxgUY4azvh/0ZZHQYnlzhtN1IQMPnyeIsBhYMMjpQsSdX/lw4wZv7kjo2wFTfr4bqIeQHFAAPpbcOol+7XYjVxsDA8P18/Pfn7UTp+drt8v15+//////////9eTsJqqF6zse/dRBFVsOEzPmx9SLDixsoZkhocHjrE/IAkpMwvcGCqeHfkr0/YhgYXtzgNFxIwGxkz6A5nXwAABUjZTuZSgdLAAAAAElFTkSuQmCC"
    },
    "GRENADE_TIER_2": {
        "name": "Large firebolt",
        "description": "A more powerful version of Firebolt",
        "meta": {
            "action_type": 0,
            "action_max_uses": 20,
            "fire_rate_wait": 50,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAmElEQVQ4jWNgGFQg+YrWf4o0/////z/FhiDzNUo8/+tOM0ERQ+cz4jJMo8TzP6vSawYzu28Mpw5xMVzOOoNTLQbQnWbyH+YlmLc0SjyJ8xq6ZhjQnWaC1RAmdIHf90QZTh3iwjDYzO4bA6vSa+K8oFHiieEF9MCDAZwBozvN5D8sAH/fE2W40bOdcCCiRyMufw8cGFyugQEAoZtrsxh1CckAAAAASUVORK5CYII="
    },
    "GRENADE_TIER_3": {
        "name": "Giant firebolt",
        "description": "The most powerful version of Firebolt",
        "meta": {
            "action_type": 0,
            "action_max_uses": 20,
            "fire_rate_wait": 80,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA10lEQVQ4jWNgoBZ4d+j/f4o0/////z82Q3pjsBuMIY7LBZem4HcZE4whZMfIiG7D5gaIZmTb0A1E0YSsWU2FgUFehIFBRo+B4cklBobdJxgYipcwYlWPofnSFEh4wMC7QxDXYAsPFmSNrhYQtoweA4OgLUKRoC0Dg/wl7BZiOGlzw///8iIMDLrZqOLvD0O8wsCA6h0WdANu3YG64jDCFTDN2MIBZyC6WiAC8OEbiMHYApEJWROMXbyEkXH3CdyacSUuDEAoEaG4ABvYfQKVD0tYFNlIdQAAdCWIvrTsB2UAAAAASUVORK5CYII="
    },
    "GRENADE_ANTI": {
        "name": "Odd Firebolt",
        "description": "A somewhat peculiar bouncy, explosive bolt",
        "meta": {
            "action_type": 0,
            "action_max_uses": 25,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAo0lEQVQ4jb2RvQ0DIQyFTZQV2Ifi9kDCw1AwA16JMagokUunSRC5n8DdSXkVQn6fn22Am1Kzhc45+byJaNrXzL162HPGHGMERGx/MUYAACEiNQS84woRqb7zafVj9KDHXnENixSjd7sh4u8l1rAIZy+c7AYwNQJn36Jy9lLDIv0Ih5BitHCystYaMoy+ASR7uI8hhLO/djZO9iv6aRWjr5v/rhd8q7dsXcWwPQAAAABJRU5ErkJggg=="
    },
    "GRENADE_LARGE": {
        "name": "Dropper bolt",
        "description": "A very heavy explosive bolt",
        "meta": {
            "action_type": 0,
            "action_max_uses": 35,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA00lEQVQ4jcVRPQvCMBS8SgcH6VAQVxfJFBDcnRz8O53b0b0/xbVDp/6CglPp0K6l4BALzRB4Dn6gTVpRKN6W9+6OdxdgLAiPUc5dyrlLwmP0lfhy3GgC02xc/C/CENG0s/rIj7OruMbqdO7lGRfCY2RtZ7DXewCASiNQ0sAJM41vG13v4unyAACQABQiCDAymbwh5y61hU9dtIVv/I1Jd7DYzXvNq7jWZpoBcMssy+D5lmUAlUbGMrUOnDCzBBgpRJAvhpQ0xqsGC8m5S49IH8v7FVcpvnV7o6EM+QAAAABJRU5ErkJggg=="
    },
    "MINE": {
        "name": "Unstable crystal",
        "description": "A crystal that explodes when someone comes nearby",
        "meta": {
            "action_type": 0,
            "action_max_uses": 15,
            "fire_rate_wait": 30,
            "speed_multiplier": 0.75,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA/0lEQVQ4jWNgGHTg//sP/0lRz0SpISgG/C+tgWvEZQi6OKYL2noYGBgYGP4tWkOUy1AM+HT1OsOnq9cJakZ2KYoBr15cRNH0d+IcuEK4oVAXYjVA9dwdRgYGBoYvO7djNeiVfzjDq5OnGe7sXYHdAGyueGnn/p8pLgTO5+DhgVvEwMDAwIJu05u1qyGG7ISIiW1cCbEpLoRBhIGB4d7CDtxeeLN2NQMDAwODUnwFw6sXFxlevbgI9/sr/3AGBgYGBpW9Z3AbIH5oJyPMIKX4CoS3oJrvLexg+LdoDcNLO3fsCQ1Z4qWd+/+/E+f8v22k8v+lnfv/20Yq/9HVDA4AADKZgAQcNsOqAAAAAElFTkSuQmCC"
    },
    "MINE_DEATH_TRIGGER": {
        "name": "Unstable crystal with trigger",
        "description": "A crystal that explodes and casts another spell when someone comes nearby",
        "meta": {
            "action_type": 0,
            "action_max_uses": 15,
            "fire_rate_wait": 30,
            "speed_multiplier": 0.75,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABXUlEQVQ4jc2SsUtCYRTFf08kFBQKIhcXxTYDcWrR9Y1vEdz0L3AMhFoLhxZxddGpUAddwsEll4aIBMVBscXB9xAKjVzKr0H98lNbo7vdc+8999zDhb+Mac4jNjHbeiJe37YaAGYv5wLAHjpVcgBtF4F2sK/gALOmIfAGYdjGGanKulQgzi4k67qSac4j5DCAN8isaYjVOcoJXF0DMC+WJeROmZozUtUYthfAUoE7ZWoKwaTTZdLpKsPrHjiCBRi2cdROFA8kgTVqKUNf2bxs2qv5APh8fgDA6bvc9uD4qa8BvNfvdhJZRpyPUoh+40apKx5sqjCjurAlYjJ3uFxy0Srs65vGldKCpL7Ajqq3iy2JGIfAoJBhM6SCcaUEgD+Zxhq1sEYtaahlxAEINB5/J/Dc17UVkT+Z/jlrOTwoZJgXy5hRfee3KgUzqouvbF70wgFhRnXRCwfEZs//iW+a8J9wTpYXcAAAAABJRU5ErkJggg=="
    },
    "PIPE_BOMB": {
        "name": "Dormant crystal",
        "description": "A crystal that explodes when caught in an explosion",
        "meta": {
            "action_type": 0,
            "action_max_uses": 20,
            "fire_rate_wait": 30,
            "speed_multiplier": 0.75,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAaklEQVR4nGNgoAp4U3L9////WKWYsKqGMLDqQdcAV41LD4oGNNVY9bAgc548eAJhKFipfOx/8OfZD8JOIgiGgwaUUDp29QScYaVtQdiGrOu1mCoM1rjicxKaHjTVOME0zeYLIbuJUkoQAADI1iuP6EAz8wAAAABJRU5ErkJggg=="
    },
    "PIPE_BOMB_DEATH_TRIGGER": {
        "name": "Dormant crystal with trigger",
        "description": "A crystal that explodes and casts another spell when caught in an explosion",
        "meta": {
            "action_type": 0,
            "action_max_uses": 20,
            "fire_rate_wait": 30,
            "speed_multiplier": 0.75,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAzUlEQVR4nGNgoAR8niyOJvKm5Pr///+RRZgg1Pf71QwMDCwGFnA2RDWEgayHEc76ftifQUaH4ckVTtuNyKqFuzWgShkZGRgYWCAuYTGwYJDRYWBgYJDR+X6Y4c+FEz8fojv4////jBBNmDZcCNkNEVSwUmFgYPjz7AeEK9KjifADp+1GiGq4H7ACqAZOxVYGBoY/F07A2QQ0QABv7ks8SrFoIAaQrIEFq+ixqyfgDCttC8I2ZF2vxRQ0WOOKz0loeiCqCYNpms3wSCQTAAA6REvOXXHfGAAAAABJRU5ErkJggg=="
    },
    "EXPLODING_DEER": {
        "name": "Summon deercoy",
        "description": "Summons a seemingly-innocuous deer",
        "meta": {
            "action_type": 0,
            "action_max_uses": 10,
            "fire_rate_wait": 80,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAnklEQVR4nGNgGDCwtzYIjQEBLLg0GBd77cUmzoRLg4BACgMDQ9bq2wwMDOoauugaPnyYg1XbtFBVBgaGAC2l8iB/hIYPH+ac7d2GyyoGBgY3bWY3bWYIG+EHNM8hA+fmdShOgjgXDoyLvfDYhuJpiFI05+26+henBkyfoKlG17Dr6t9dV/86N6+DOBpCdq7biF0DVmkCfiAGkKyBZAAASWMv2tHU6zsAAAAASUVORK5CYII="
    },
    "EXPLODING_DUCKS": {
        "name": "Flock of Ducks",
        "description": "Summons a chaotic flock of spicy ducks",
        "meta": {
            "action_type": 0,
            "action_max_uses": 20,
            "fire_rate_wait": 60,
            "reload_time": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABA0lEQVQ4jWNgGBkguMLxf3CF439sckzYBLszPOCKgysc/++Zfg6n4VgNQDfEJdOIgYGBgeHJhqT/MAyTY8SmOcZV/7++siQDAwMDwwmBnwwMDAwMrx68Y+jS/czAwMDAYFl9D64PxQXzJtb+Z2BgYFiy+yJcgcUHdgYGBgYGMQUhhrLLvCiaGRgYGFjQbZ83sfb/2+unGS7efc4AcwU+gOKCt9dPMzAwMDAIa5oyOPn4MVy8+xzuCphL8BpQOmMHI8wQBgYGDBccXnERI8ywBmJ3hsd/YU1TBjf5pwzLdzxjKJ2xA6s6nOB4qxI8mpCjDBvASAfImhkYGBhkAuaRZjvdAQDpwFKjUy9D6wAAAABJRU5ErkJggg=="
    },
    "WORM_SHOT": {
        "name": "Worm Launcher",
        "description": "Summons a giant worm to cause havoc for a moment!",
        "meta": {
            "action_type": 0,
            "action_max_uses": 10,
            "fire_rate_wait": 80,
            "reload_time": 40,
            "speed_multiplier": 1,
            "spread_degrees": 20,
            "action_mana_drain": 150
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA7ElEQVQ4jWNgGAVEAU0t3/+4+EzIEk4xs/4zMDAwWLiU/kdWbBhbCpfT1PL9L2nkC+czwhT+/////+PTzQwVex0YXly9wSChrcHw4uoNhufnNjNIGvni5MMNgNmaYLaWwdpEluHomccMh3gXwA27t3czAwMDAwOXhCoKnwlm+/HdXQwfn91gsDaRZchfF81gbSILV7S0wpZBydmXgUtCFYOP4oKPz24w5AdcJckFzDADHt892uBndodh7tMChjWHpBjeyqTA/czIIswga27N8PP7fww+CsAVCxYupf+RY8EpZhacTxTAlw6GAQAAddyKHgSby0QAAAAASUVORK5CYII="
    },
    "BOMB_DETONATOR": {
        "name": "Explosive Detonator",
        "description": "All nearby explosive spells cast by you instantly detonate",
        "meta": {
            "action_type": 1,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAzUlEQVR4nJ1QIQ7DMAy8VP3BBvuARSOD0/h+0FcUt7AocMN5xFQ6VF4VllQpHCjs3pABT1bkdAM9RcpdIvvOBjZjKV3IvfcsQ57QZbWZX3NcyZJrUroKV8MBwJC39KKUEhG890q8Wm0AXI7nU3ONTfZ3/dVD3oYp48ahTAB0Yx8HYLyraf0j9PnjmTLrxj6rspjzDLvbYSUIbynEUjo669nEB7WQBVYb1rRcRPshJMwez4ZI4Wpq+XM/oQ+1t9oMeUthyFwOIPJQgRhjIz6hs3k8fbFe3AAAAABJRU5ErkJggg=="
    },
    "LASER": {
        "name": "Concentrated light",
        "description": "A pinpointed beam of light",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -22,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAmUlEQVQ4jWNgoBb4DwWb73v/J1szOYYwYRPkV/BkwGdI92o7uBwjAwMDw+b73v99FLYwMDAwMBxmmIqi+OOD7Qy+ilsZcRkGl8BlI7+CJ1bDulfb/S8NPcSI02R0Q3EZhNcAXIbCDLNjzMHvAlwaSXIBLo1E2Xjo/5T/h/5PwUgXGNFIcxuxAZgrGBkYGBgO/Z/yn2QbBw0AAOGLfBjjKtqaAAAAAElFTkSuQmCC"
    },
    "MEGALASER": {
        "name": "Intense concentrated light",
        "description": "A spectral wand is summoned that casts a huge beam of light",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 90,
            "speed_multiplier": 1,
            "action_mana_drain": 110
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA10lEQVQ4y6WTOQ7CMBBF30QcAuVI3AAJGkqnyFWggC5pKKmTltMk4hafgtjKnqCMZNmyZt6fxTZJbLHdH759JQOIphxLEkoSTQSHuwjgfLPgkNfWV5mrURHAM5WVJAB6v7AWcLFBnRK8+jF1q4IBTL8xGKAmCwAO3Mlr4xIvcCQhiaxChZwkKatQVqEVFgCDQL8XclMwAsArj0GaM1NrtIR2Bksgr76qF2MgCjkkcboyAI2V1QeZ/0x+ZO2X2BqhvM8+dmHMgHUAc3aJRUmiT/0ACCDb+p2/enJxlYT42hYAAAAASUVORK5CYII="
    },
    "LIGHTNING": {
        "name": "Lightning bolt",
        "description": "The primordial force of nature",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 50,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAATUlEQVR4nGNgGCGg/donEqQh3P///2FRiibafu2T3aar2E39//8fmknt1z79//8PLo5iM5qo3aarEKUQKQgXnx+weoOwHkw3k6ZhWAEAsN1H0BhtiugAAAAASUVORK5CYII="
    },
    "BALL_LIGHTNING": {
        "name": "Ball Lightning",
        "description": "Summons three short range electrical orbs",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 50,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAFyGlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4gPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgNi4wLWMwMDIgNzkuMTY0NDg4LCAyMDIwLzA3LzEwLTIyOjA2OjUzICAgICAgICAiPiA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPiA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIiB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIgeG1sbnM6cGhvdG9zaG9wPSJodHRwOi8vbnMuYWRvYmUuY29tL3Bob3Rvc2hvcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIgeG1wOkNyZWF0b3JUb29sPSJBZG9iZSBQaG90b3Nob3AgMjIuMCAoV2luZG93cykiIHhtcDpDcmVhdGVEYXRlPSIyMDE5LTEyLTA4VDE2OjM2OjU2KzAyOjAwIiB4bXA6TW9kaWZ5RGF0ZT0iMjAyMC0xMS0xMFQxNzo0MzoyMyswMjowMCIgeG1wOk1ldGFkYXRhRGF0ZT0iMjAyMC0xMS0xMFQxNzo0MzoyMyswMjowMCIgZGM6Zm9ybWF0PSJpbWFnZS9wbmciIHBob3Rvc2hvcDpDb2xvck1vZGU9IjMiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6YWEyYzZkMWQtYjU3ZS1jYzRjLWFmNjktMjI1OGRkNzg3MmZiIiB4bXBNTTpEb2N1bWVudElEPSJhZG9iZTpkb2NpZDpwaG90b3Nob3A6YmE5N2QzNTItNWVhZi0wYTRjLWE4ZjEtMGRkNTI0YTFjNzg2IiB4bXBNTTpPcmlnaW5hbERvY3VtZW50SUQ9InhtcC5kaWQ6MmFmNjM5ZDktMzMyZS1jNTRhLThlYjUtMmIxZTExMDY5ZWRkIj4gPHhtcE1NOkhpc3Rvcnk+IDxyZGY6U2VxPiA8cmRmOmxpIHN0RXZ0OmFjdGlvbj0iY3JlYXRlZCIgc3RFdnQ6aW5zdGFuY2VJRD0ieG1wLmlpZDoyYWY2MzlkOS0zMzJlLWM1NGEtOGViNS0yYjFlMTEwNjllZGQiIHN0RXZ0OndoZW49IjIwMTktMTItMDhUMTY6MzY6NTYrMDI6MDAiIHN0RXZ0OnNvZnR3YXJlQWdlbnQ9IkFkb2JlIFBob3Rvc2hvcCAyMi4wIChXaW5kb3dzKSIvPiA8cmRmOmxpIHN0RXZ0OmFjdGlvbj0ic2F2ZWQiIHN0RXZ0Omluc3RhbmNlSUQ9InhtcC5paWQ6YWEyYzZkMWQtYjU3ZS1jYzRjLWFmNjktMjI1OGRkNzg3MmZiIiBzdEV2dDp3aGVuPSIyMDIwLTExLTEwVDE3OjQzOjIzKzAyOjAwIiBzdEV2dDpzb2Z0d2FyZUFnZW50PSJBZG9iZSBQaG90b3Nob3AgMjIuMCAoV2luZG93cykiIHN0RXZ0OmNoYW5nZWQ9Ii8iLz4gPC9yZGY6U2VxPiA8L3htcE1NOkhpc3Rvcnk+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+kTgeHwAAAQ1JREFUOI2lkzFuwzAMRT+DjvFBdATnABm7FgW6tjcooM3eLOQGbsYORdduvUCOYN3DzuzfQWIsNU6dIgQI2YIofj5SQhK32GrpgPPD3xlIXuVN15McGdZpfyFwZBo8+RUXNF3PpuuhGf+lIAnK9re7d5JEWbUsqxYkc4gBGAlAnB/OeH2/Pkn81BV36QFr1nD+KPhlm/qNAORQv8D5gYf6Gc4fw0UzNaOs2qTWMcrV+nOYK5VuTSHWrAmAAOTr80O0nKBiL9YUEjOrAmh2NF1/AlNWLWLW6OdA1YUkEmC0pkgY6BBKxmFhEkdqm9J2Tv/5LMzKUmhpzy9N48XHtKn3p57fPzxyFiAQGNxiP1KexEdNKsnGAAAAAElFTkSuQmCC"
    },
    "LASER_EMITTER": {
        "name": "Plasma beam",
        "description": "An instantaneous, dangerous beam of light",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 6,
            "speed_multiplier": 1,
            "action_mana_drain": 60
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAf0lEQVQ4jWNgGAUoQCZu6XaZuKXbSdHDiKxZTNnAg4GBgeHV3Qs7niyK9iTGABaY5sykKI8qe4hg20Etj+kMDARdArfEqPHqf3Sw6wMCw/jocnAXYAOu/Aj27o8I/u6PqPIsMD+3HdRC8gIDw/R5y3YQEwYUByIKICcaRwEDAwCf5mLeReBJeQAAAABJRU5ErkJggg=="
    },
    "LASER_EMITTER_FOUR": {
        "name": "Plasma Beam Cross",
        "description": "Four deadly plasma beams in a cross-shape. Look out, they can hurt you as well!",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA3UlEQVQ4jaVTIRKDMBDcdOoxDAqHq0LgqWEGX4dA4PhCq+kXcHXUoWHAwA+qcP1AFbzgKjKBiCaFdtVdcre528sBGjQjUTMS6WKUyDqakXVqkp3qouwHAEA7LfYmgtfzUQNAYCz2JgIAXjst9i8Eq7AHuGBlP8ilemkSmYHBnTSJzByoRJLluOHJP+ByZIw1I1FgcLFEwidI7UDEAwATc5YPha8jE/FM18LZ5861B/LbfZ6E3ILyFTsuKiL+E+24qFRxf0/hK4FOCy2B5bihENVy3HBbXVi/TFqsWec3/Ll9Nrs7krYAAAAASUVORK5CYII="
    },
    "LASER_EMITTER_CUTTER": {
        "name": "Plasma Cutter",
        "description": "A plasma beam specialized in cutting materials!",
        "meta": {
            "action_type": 0,
            "reload_time": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAtklEQVQ4jd2RMQqDMBiFv4inEBfBUSQIhV7ArT2ES2/RyUN09CAdXQoFCeIouEivYSfDr8S6dGkfBPIn/3v/ewn8PJQs8jKaZH2/Dood2Ia8jKZYh6RZAkDbdPRm3CTO4kqSL6cCTQGAoXISH6+nHXA718qfL9IssWRgsZfQQSHEazyAWIccg8Oi0VDZ9cmR7zxdOTBUm9E8gN6MNtue2DqaddA2HSbYnjRDPiJ86xuliKvpz/EGBXZIafmI210AAAAASUVORK5CYII="
    },
    "DIGGER": {
        "name": "Digging bolt",
        "description": "A bolt that is ideal for mining operations",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 1,
            "reload_time": -10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAnElEQVR4nGNgGE5g89yIwiTjzXMj8CtjhrM+f/kW6q3HwMAgIcJenWMf5a+zfNMVTA2MDAwMhUnGTtaqEP6+o7chbGRG/7yzKDacOP9cQoRdUU545tJTEEvQtEX560iIsJ84/5yBgYEJrnXm0lPp0WYQBpolcBF8TkLWv2XfPeyh5OOktHluBJyBNZQY4SwfJyWIkb7JK3AE6XABAFiLShsXU2bTAAAAAElFTkSuQmCC"
    },
    "POWERDIGGER": {
        "name": "Digging blast",
        "description": "More powerful digging",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 1,
            "reload_time": -10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAzElEQVR4nGNgGGrg+25nnHKbk8ObXaw3J4fj18wE5y88ed5AXoaBgQGiDVnn9+ftcDYLRAVEKQMDw4WHTyDsCw+fQPRcePiEU7ISpmcvEwMDQ+2eoxcePkG2BFmbgbzM5uTwntgtEBNRnBRvboip7cLDJwtPnocrY4Q4iYGBAc3shSfP60uIMjAwXHzxes2VWxAncUpWMsK1NrtYX3zxGq5IX0K0ds9RuKc5JSu/73bmdN3LAtcAVw1Rt+bKLbgUp2QlrrBGB/gibtAAABdvZRg5vhlIAAAAAElFTkSuQmCC"
    },
    "CHAINSAW": {
        "name": "Chainsaw",
        "description": "A good tool for cutting meat. Also has some magical properties...",
        "meta": {
            "action_type": 0,
            "reload_time": -10,
            "speed_multiplier": 1,
            "spread_degrees": 6,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAjUlEQVR4nGNgGNHg0KxZh2bNYmBgWD9nDlYSAhgZGBgWp6fLGxtD+FJWVliNu3ziRGBKCgMDAzMDA8P6s2edpKQEpKQYGBhYhIT+/P6NjB7fuXNyzx5rT09HI6OVmzYxEXTqrYsX1fT1GRgYIDYwQZwEl+bk5kZDHpGRSlpaKjo6U6qrsXgaIoqLHKkAAFVmQSkQ1m2hAAAAAElFTkSuQmCC"
    },
    "LUMINOUS_DRILL": {
        "name": "Luminous drill",
        "description": "A pinpointed, short-ranged beam of concentrated light",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -35,
            "reload_time": -10,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA4ElEQVQ4jWNgoBAwMjAwMDDVMiQhC/IJMcjD2B8KGeoJGgADMINgBqhLM4jB5E6GMWQSNICBgYFBoJ+hEV2zs5iYAIzd5vAqEsVSbKbi0qwsoyc/947LMYIG4NIMYyMbgmEAzHZcmhkYGBislKNlYYaw4LMZm0YY21e5TYeBYQ+mAYQ0CzNo84symPLCxFEMqDogthxdMzaN8+66Hk9duMcSwwBiNf5rZrDC8PfcOy7H5t5xOXb9//zH1//Pf/zq/6lP/6Fg7h2XY0y1DP+xBBcCkK0RppksjcgGkKWRGgAAmgF9ONAr914AAAAASUVORK5CYII="
    },
    "LASER_LUMINOUS_DRILL": {
        "name": "Luminous drill with timer",
        "description": "A pinpointed, short-ranged beam of concentrated light that casts another spell after a timer runs out",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -35,
            "reload_time": -10,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABaElEQVQ4ja2RTyiDYRzHP+9rOfnzFnvbYVJ2GZOWUJZQe92WRA5yUw7KAaclEpa4Oax2ev05UVIO3N7DDnqVpB3WOMxpVtIOS47yunjWa3tR43v6PX8+n+fp94M/RgKQV5lpzyR0xdNTOig+3ZAfmNsoLrL2k6AGoMOduFU8PWzPd+KvNRkfHeQy46I20z3Utn7h8U4SyZ9w4SRwAZTg5lf8mkYwssDxwS7ROLwBYVVVwkmOALaGn6fsAlkUj6kzAIKRFVLnsdI6rKqKuOPzdrXqWc10FHiDYwCkzmNf1nZY1HaJLBoWjae5L9RhGAb3hTqi8TTFp5sKGCDkm24REklsBiYSVvkUlnZOr8pBUTcRaFSlvgaJsiwn1aPvXhWgm956AEmSJNdvsBO49zByNXto9MPnGJ0a9RP4vkmo/OfoWc3Us5p5Z+3n7qz93LN1/WJ9Rs9qpryKVQHZUzUo4KpAu6Aq8D/yAYlYtNQiUHmcAAAAAElFTkSuQmCC"
    },
    "TENTACLE": {
        "name": "Summon Tentacle",
        "description": "Calls a terrifying appendage from another dimension",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAuElEQVR4nGNgIBEwovEz09shjOkzKwlogCuFA6x6mJBVy8hKPXx0U0ZWSkZWCqsRUBvgqhkYGKpr4iESrS0Lnzx+hmkP1AYZWamjR/fDVUN0otkDYUBtQDMeGaBZxQIRPXp0v7W1I6ZqTFOgTsKlGifITG/38kxobVlIUCUTnAWxxMszAb8GlIiDeJ0Bh+/RbZg+s/LJ42dHj+5nYGDA4zYmZM70mZXycuoMDAwQbYQ1wAHJgYYHAABEnkGl41V75gAAAABJRU5ErkJggg=="
    },
    "TENTACLE_TIMER": {
        "name": "Summon Tentacle with timer",
        "description": "Calls a terrifying appendage from another dimension! Comes with a timer",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAABRklEQVR4nGNgIBEwovEz09shjOkzKwlogCuFA6x6mJBVy8hKPXx0U0ZWSkZWCqsRUBvgqhkYGKpr4iESrS0Lnzx+hmkP1AYZWamjR/fDVUN0otkDYUBtQDMeGaBZxQIRPXp0v7W1I6ZqTFOgGrCq1g6eLiBhAmF/eHHm6tpMBgYGZgYGhjNn96qrml++ciYhoXDfvg3IqjtydDTYjgX52R25xsInZ/T6+lYmuHkQS7w8EyBcqGqRLy4uLhEJBR05OhDboBqmz6yE+Mza2rG1ZSFE8MmFDQwMDAY+NRe2tEDYCA1wPUeP7oeEDAMDg4xBAAMDw4UtLXA21A9wcObsXhfncD5+3qNH97/8yXTkGouBjsyFY9t+8elVTLny4cWZ19e3omhgYGAwNXHh4+eVk1NcMTWFT87oyDWWU4+E9px6BQ8lkgEALvGGsg2NWsAAAAAASUVORK5CYII="
    },
    "HEAL_BULLET": {
        "name": "Healing bolt",
        "description": "A magical bolt with rejuvenative powers",
        "meta": {
            "action_type": 0,
            "action_max_uses": 20,
            "fire_rate_wait": 4,
            "speed_multiplier": 1,
            "spread_degrees": 2,
            "action_mana_drain": 15
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAh0lEQVR4nGNgoDVgxBTafN8bzvZV3Iomy4TVGB+FLT4KW7BKYdGgoWCAxiBsAx7AAqEg7sY08vb/agYGhhsPLjCg+Wfzfe//eAE8JMh1EgMDw5YHPnAnqTC0MDAw3GGogXAhTsIJbv+vhjgD4gE0QLKTsGiAOwCrS1gwhRgYGLY88CHVZuoBABgxSK9eHv3BAAAAAElFTkSuQmCC"
    },
    "SPIRAL_SHOT": {
        "name": "Spiral shot",
        "description": "A mystical whirlwind of magic sparks",
        "meta": {
            "action_type": 0,
            "action_max_uses": 15,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAh0lEQVR4nGNgIBEwYhX9////phd/GBgY/CVZSTMv5sAnAiok+19CEGE9MQc+IauDs5EFibUaWQ8LAwPDxue/ifccEy6TMMH///9RWLhUw8U3Pv8NtYGREXtsQMDzQnGcTsIDYg58wuJPrEEp2f8SX1Qgq0OLQcJ6sAIs3oWEhp8EC/7AIBYAADkkP/muaDnBAAAAAElFTkSuQmCC"
    },
    "MAGIC_SHIELD": {
        "name": "Magic guard",
        "description": "Four guarding lights rotate around you for a time",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABJUlEQVQ4jcVSsWrDMBB9Kd1CB0G8daohkKF08KIlYwmKfyFDIWPbOd/gOe5o6JBfsEXoD2gxuHQwBPQFCdZgPF+HoCInTvGWt0h3ujvevSfg2hhcenh8+VoPJ3d+U9b65/P5vfcAHqmMjTwBAOawl2rF5yLRFAYMaW4gl36r58YNRKJpMRsLGzdlrQEgDBhenxjCgIFHKnN7bt3AFgLAZruTlnqaG9jTsutkkOYGH98GaW6gVnxu83LpDzbbnXT1OROjM9lRwyOVna4B4Cgej1TWd5C9/2nQlLW+nz68samH4UT57grAUeBLTrSKRKIpLioSiSaXXVxUREQUFxW5PS0XzGEvF7OxsE6YSGVNWWs28oTrhIszKqdUuz7WvwNc9P3O18UvupadEeNzln4AAAAASUVORK5CYII="
    },
    "BIG_MAGIC_SHIELD": {
        "name": "Big magic guard",
        "description": "Eight guarding lights rotate around you for a time",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 60
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA10lEQVQ4jbVTsQ3CMBA8pUUUkWABUlG4oskKFktYME3qpPUYMUUWSBMpEgWVV0gKRP1UjhzzMYoE17198t/f+YF/QKimzIvWCNWUACC1vQNAXrTGryf4BKktSW3JkR2qfiCpLVX9QP55AgC3ayaqfqB0t5fnUwoAeD2e1pHyojV1N8JcDqi7EeHjU2ciYruHCqS2FN4vSlwa0fnzQWBN8uAM5lSuwkzBrFiJBAA2x23m584hOqZQTRnmz/kQMxp+RBzha9QxglMYVRCTyH3x6D74dbhYP8cbAJnzmtiDhdwAAAAASUVORK5CYII="
    },
    "CHAIN_BOLT": {
        "name": "Chain bolt",
        "description": "Fires a mysterious bolt that jumps from enemy to enemy",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 45,
            "speed_multiplier": 1,
            "spread_degrees": 14,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAlUlEQVQ4jb2SwQkDIRREZ/sIpKRgIykgBViNBzvwmDoEtwVvOjksEzYQJfkbMuBFnPf/4AC/UPaF2Re2RO7vRm+HoOAiW+ILyKQ9qPYNNp08AwUXWfsGMkEE0ibSV7DayeCibXpL/zYr26HJhzKbzdmX53/Xbmyj8qtAaqPOu80WGc+30zICy+iuFwBAua8AgJnnYz0A4VapNAGm2NkAAAAASUVORK5CYII="
    },
    "FIREBALL": {
        "name": "Fireball",
        "description": "A powerful exploding spell",
        "meta": {
            "action_type": 0,
            "action_max_uses": 15,
            "fire_rate_wait": 50,
            "speed_multiplier": 1,
            "spread_degrees": 4,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAXUlEQVR4nGNgGAWEASMeuY89UWzeghD2r63v+UuWMTAwsED4rwMM2Ww0+YqXfuqNZmBggKvj0JgCYzB89xbk1JwKVQ3X9rEn6j8O8P16Nk4nfeyJQrYH2UmjgBgAABsbMx56SSayAAAAAElFTkSuQmCC"
    },
    "METEOR": {
        "name": "Meteor",
        "description": "A destructive projectile from the skies!",
        "meta": {
            "action_type": 0,
            "action_max_uses": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 150
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAABBElEQVR4nJ1SMYrDQAycNSKYg+CrU20fFwc2xmWq9GnuA5fiXhDYF/gLKVzkB/6CSxOcLkXKfUIWNy6W+Ao5W+z5TDgVYiQ0Gkm7wD/MqPS2XQFg/yrHqJTBfGXg0OJzbVQaZMM8RzjUVQnJ2Oor+8dZREU7p/A4CwAkYwDhZh9kQ1clcwpOhGRudUMyB9DXpSclvAZGpW+HLwCOY3Vj9XW5u/gjjXE2WN0ACD9OnCGZk4zdeOQRlrtLVwEAJADQ+zdjkrlRIipaX4G352HG6icIsmFiJABR0fZ1+Ts/vYMzqxt7P474fsTz7v6VnHVVEm72Luzrkg/1JwEA/xTuPfnqL9kP/AlhLrQVMHgAAAAASUVORK5CYII="
    },
    "FLAMETHROWER": {
        "name": "Flamethrower",
        "description": "A stream of fire!!",
        "meta": {
            "action_type": 0,
            "action_max_uses": 60,
            "speed_multiplier": 1,
            "spread_degrees": 4,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAjElEQVR4nGNgGMrguhkfUdIE1GGCr636JNh+3YwPzSoWZHXfr2dDGP/WHfnqz/Bo4305f0Xu6ovXzfg0T32S81dkOHURYe/369n///////8/hA0XgXMRnoErhQCsXLhqFCdBAIfGlB83ciAkxHnfGAw0T31CDwq4wXD3QJyBFqyMWD3NqTmVcFAOGgAATht5HLAkL5EAAAAASUVORK5CYII="
    },
    "ICEBALL": {
        "name": "Iceball",
        "description": "A magical ball of frozen fire",
        "meta": {
            "action_type": 0,
            "action_max_uses": 15,
            "fire_rate_wait": 80,
            "speed_multiplier": 1,
            "spread_degrees": 8,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAZklEQVQ4jWNgGAUDDxiJURQ77fd/zzAWOH/7qj8Mi7NYGVEMsA79/F/JkYNhUSYLQ9z0PwwMDAwMyJoihVENXf6WgSFKhJERrhnZoNhpv/8TAsve/P9P0Aux037/R3cJuhdGwWAAACPcQmUUl97vAAAAAElFTkSuQmCC"
    },
    "SLIMEBALL": {
        "name": "Slimeball",
        "description": "A dripping ball of poisonous slime",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 10,
            "speed_multiplier": 1.1,
            "spread_degrees": 4,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAf0lEQVR4nGNgGAWEASMav2hyUF/uOgiDgYFBSPYHAwODhBRPitkqLBogiuDq3j3m0DJnMZDlufD4CwMDA0QPE7KGvtx1EOMlpHjePeZAlnrx7AuEwYLskqLJQUKyPxgYflw7yaFlzsLAwHDt5B8Ghi/Y/QB3PYQNcRWaB0YqAABoJytGPJt3lQAAAABJRU5ErkJggg=="
    },
    "DARKFLAME": {
        "name": "Path of dark flame",
        "description": "A trail of dark, deadly flames",
        "meta": {
            "action_type": 0,
            "action_max_uses": 60,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAjklEQVR4nGNgoATMduwnQwqL9GzHfiMNWxRBZA6yNFwczmCCcKY/XwdXkbq/MFMyCM3O1P2F2N0A1waxBN0lEBvgBsCtmv58HS6PMaIJYVWaKRkEdxITQdVogAWrKNzTmEYwYShmwAwi7BrgoQk3FasLsTsJWSmyjxnQQokBFnZwDWiqsWhA1saAGcFkAABdFU4Rm4HUkwAAAABJRU5ErkJggg=="
    },
    "MISSILE": {
        "name": "Summon missile",
        "description": "A missile!!!",
        "meta": {
            "action_type": 0,
            "action_max_uses": 20,
            "reload_time": 30,
            "speed_multiplier": 1,
            "spread_degrees": 3,
            "action_mana_drain": 60
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAbklEQVR4nGNgGIGAEUKdNzZmYGDg1NLSWLwYvwYmZM73a9duxMYSZQMDA0N7ezucraCgAGFo9PTcKCmJjIzE7iSIq857ecGlDbdtg3DheligEmfPImx48ABuw/dr1xi8vAjYgN/fKJ4mJpSGAwAA0iAiz/Cf4F0AAAAASUVORK5CYII="
    },
    "PEBBLE": {
        "name": "Summon rock spirit",
        "description": "Summons an autonomous rock ally",
        "meta": {
            "action_type": 0,
            "action_max_uses": 10,
            "fire_rate_wait": 80,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAA6ElEQVR4nGNgoBZIrZyAVZwJl2oR55dY9WDREBBf9EJ5b5guk4jzS0xZRqwuef3s0V/b2xJ3nRkYGGa3F+C0oXJPJUS1qJQc82FVrJ5BaAiILwrTZXqhvBfC1Yxlh2gWcX4ZEF+E3Ya6zZcgBos4v4Ro1oxlD9NFUYPCgTgarqjJV+/KgytonkTRAHE9siLmw6p1my/h1MDAwHBi7wa4Igj5YNZnZAUsyJy7188paxppxrJfX6xax3CJ+bAqRGTDwj4sNkBE4R6AqIaYghxKCBAQX6Rr5gBhwEUgDIg4OoCLIktjV0oSAACDA1+k4ORtWwAAAABJRU5ErkJggg=="
    },
    "DYNAMITE": {
        "name": "Dynamite",
        "description": "Summons a small explosive",
        "meta": {
            "action_type": 0,
            "action_max_uses": 16,
            "fire_rate_wait": 50,
            "speed_multiplier": 1,
            "spread_degrees": 6,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAWklEQVR4nGNgGDDQlOo5pyqCWNVwpZh6mLBqePb6IxoDH9hrYQFBWC3BbgMDA8Oxd+8gjJS2FURpsBISwipO2AaiNBx79440G6yEhEi2Aas4dg14VOO0YUABAJL6In7hL+/KAAAAAElFTkSuQmCC"
    },
    "GLITTER_BOMB": {
        "name": "Glitter bomb",
        "description": "Summons a bomb that explodes into volatile fragments",
        "meta": {
            "action_type": 0,
            "action_max_uses": 16,
            "fire_rate_wait": 50,
            "speed_multiplier": 1,
            "spread_degrees": 12,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAAXNSR0IArs4c6QAAABVQTFRFAAAAgl9TnHpY0DXS/8nX4LxXhjRoZI4hxwAAAAd0Uk5TAP///////6V/pvsAAAA/SURBVBiVjU05DgAwCFIx/v/JpU7o0JTowCGa/cNj8ugR37kqADYiRahfIEp5IVMU8i2sm1vJlJSglfVm8BcObVoBAfvu4koAAAAASUVORK5CYII="
    },
    "BUCKSHOT": {
        "name": "Triplicate bolt",
        "description": "A formation of three small, fast bolts",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 8,
            "speed_multiplier": 1,
            "spread_degrees": 14,
            "action_mana_drain": 25
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAKT2lDQ1BQaG90b3Nob3AgSUNDIHByb2ZpbGUAAHjanVNnVFPpFj333vRCS4iAlEtvUhUIIFJCi4AUkSYqIQkQSoghodkVUcERRUUEG8igiAOOjoCMFVEsDIoK2AfkIaKOg6OIisr74Xuja9a89+bN/rXXPues852zzwfACAyWSDNRNYAMqUIeEeCDx8TG4eQuQIEKJHAAEAizZCFz/SMBAPh+PDwrIsAHvgABeNMLCADATZvAMByH/w/qQplcAYCEAcB0kThLCIAUAEB6jkKmAEBGAYCdmCZTAKAEAGDLY2LjAFAtAGAnf+bTAICd+Jl7AQBblCEVAaCRACATZYhEAGg7AKzPVopFAFgwABRmS8Q5ANgtADBJV2ZIALC3AMDOEAuyAAgMADBRiIUpAAR7AGDIIyN4AISZABRG8lc88SuuEOcqAAB4mbI8uSQ5RYFbCC1xB1dXLh4ozkkXKxQ2YQJhmkAuwnmZGTKBNA/g88wAAKCRFRHgg/P9eM4Ors7ONo62Dl8t6r8G/yJiYuP+5c+rcEAAAOF0ftH+LC+zGoA7BoBt/qIl7gRoXgugdfeLZrIPQLUAoOnaV/Nw+H48PEWhkLnZ2eXk5NhKxEJbYcpXff5nwl/AV/1s+X48/Pf14L7iJIEyXYFHBPjgwsz0TKUcz5IJhGLc5o9H/LcL//wd0yLESWK5WCoU41EScY5EmozzMqUiiUKSKcUl0v9k4t8s+wM+3zUAsGo+AXuRLahdYwP2SycQWHTA4vcAAPK7b8HUKAgDgGiD4c93/+8//UegJQCAZkmScQAAXkQkLlTKsz/HCAAARKCBKrBBG/TBGCzABhzBBdzBC/xgNoRCJMTCQhBCCmSAHHJgKayCQiiGzbAdKmAv1EAdNMBRaIaTcA4uwlW4Dj1wD/phCJ7BKLyBCQRByAgTYSHaiAFiilgjjggXmYX4IcFIBBKLJCDJiBRRIkuRNUgxUopUIFVIHfI9cgI5h1xGupE7yAAygvyGvEcxlIGyUT3UDLVDuag3GoRGogvQZHQxmo8WoJvQcrQaPYw2oefQq2gP2o8+Q8cwwOgYBzPEbDAuxsNCsTgsCZNjy7EirAyrxhqwVqwDu4n1Y8+xdwQSgUXACTYEd0IgYR5BSFhMWE7YSKggHCQ0EdoJNwkDhFHCJyKTqEu0JroR+cQYYjIxh1hILCPWEo8TLxB7iEPENyQSiUMyJ7mQAkmxpFTSEtJG0m5SI+ksqZs0SBojk8naZGuyBzmULCAryIXkneTD5DPkG+Qh8lsKnWJAcaT4U+IoUspqShnlEOU05QZlmDJBVaOaUt2ooVQRNY9aQq2htlKvUYeoEzR1mjnNgxZJS6WtopXTGmgXaPdpr+h0uhHdlR5Ol9BX0svpR+iX6AP0dwwNhhWDx4hnKBmbGAcYZxl3GK+YTKYZ04sZx1QwNzHrmOeZD5lvVVgqtip8FZHKCpVKlSaVGyovVKmqpqreqgtV81XLVI+pXlN9rkZVM1PjqQnUlqtVqp1Q61MbU2epO6iHqmeob1Q/pH5Z/YkGWcNMw09DpFGgsV/jvMYgC2MZs3gsIWsNq4Z1gTXEJrHN2Xx2KruY/R27iz2qqaE5QzNKM1ezUvOUZj8H45hx+Jx0TgnnKKeX836K3hTvKeIpG6Y0TLkxZVxrqpaXllirSKtRq0frvTau7aedpr1Fu1n7gQ5Bx0onXCdHZ4/OBZ3nU9lT3acKpxZNPTr1ri6qa6UbobtEd79up+6Ynr5egJ5Mb6feeb3n+hx9L/1U/W36p/VHDFgGswwkBtsMzhg8xTVxbzwdL8fb8VFDXcNAQ6VhlWGX4YSRudE8o9VGjUYPjGnGXOMk423GbcajJgYmISZLTepN7ppSTbmmKaY7TDtMx83MzaLN1pk1mz0x1zLnm+eb15vft2BaeFostqi2uGVJsuRaplnutrxuhVo5WaVYVVpds0atna0l1rutu6cRp7lOk06rntZnw7Dxtsm2qbcZsOXYBtuutm22fWFnYhdnt8Wuw+6TvZN9un2N/T0HDYfZDqsdWh1+c7RyFDpWOt6azpzuP33F9JbpL2dYzxDP2DPjthPLKcRpnVOb00dnF2e5c4PziIuJS4LLLpc+Lpsbxt3IveRKdPVxXeF60vWdm7Obwu2o26/uNu5p7ofcn8w0nymeWTNz0MPIQ+BR5dE/C5+VMGvfrH5PQ0+BZ7XnIy9jL5FXrdewt6V3qvdh7xc+9j5yn+M+4zw33jLeWV/MN8C3yLfLT8Nvnl+F30N/I/9k/3r/0QCngCUBZwOJgUGBWwL7+Hp8Ib+OPzrbZfay2e1BjKC5QRVBj4KtguXBrSFoyOyQrSH355jOkc5pDoVQfujW0Adh5mGLw34MJ4WHhVeGP45wiFga0TGXNXfR3ENz30T6RJZE3ptnMU85ry1KNSo+qi5qPNo3ujS6P8YuZlnM1VidWElsSxw5LiquNm5svt/87fOH4p3iC+N7F5gvyF1weaHOwvSFpxapLhIsOpZATIhOOJTwQRAqqBaMJfITdyWOCnnCHcJnIi/RNtGI2ENcKh5O8kgqTXqS7JG8NXkkxTOlLOW5hCepkLxMDUzdmzqeFpp2IG0yPTq9MYOSkZBxQqohTZO2Z+pn5mZ2y6xlhbL+xW6Lty8elQfJa7OQrAVZLQq2QqboVFoo1yoHsmdlV2a/zYnKOZarnivN7cyzytuQN5zvn//tEsIS4ZK2pYZLVy0dWOa9rGo5sjxxedsK4xUFK4ZWBqw8uIq2Km3VT6vtV5eufr0mek1rgV7ByoLBtQFr6wtVCuWFfevc1+1dT1gvWd+1YfqGnRs+FYmKrhTbF5cVf9go3HjlG4dvyr+Z3JS0qavEuWTPZtJm6ebeLZ5bDpaql+aXDm4N2dq0Dd9WtO319kXbL5fNKNu7g7ZDuaO/PLi8ZafJzs07P1SkVPRU+lQ27tLdtWHX+G7R7ht7vPY07NXbW7z3/T7JvttVAVVN1WbVZftJ+7P3P66Jqun4lvttXa1ObXHtxwPSA/0HIw6217nU1R3SPVRSj9Yr60cOxx++/p3vdy0NNg1VjZzG4iNwRHnk6fcJ3/ceDTradox7rOEH0x92HWcdL2pCmvKaRptTmvtbYlu6T8w+0dbq3nr8R9sfD5w0PFl5SvNUyWna6YLTk2fyz4ydlZ19fi753GDborZ752PO32oPb++6EHTh0kX/i+c7vDvOXPK4dPKy2+UTV7hXmq86X23qdOo8/pPTT8e7nLuarrlca7nuer21e2b36RueN87d9L158Rb/1tWeOT3dvfN6b/fF9/XfFt1+cif9zsu72Xcn7q28T7xf9EDtQdlD3YfVP1v+3Njv3H9qwHeg89HcR/cGhYPP/pH1jw9DBY+Zj8uGDYbrnjg+OTniP3L96fynQ89kzyaeF/6i/suuFxYvfvjV69fO0ZjRoZfyl5O/bXyl/erA6xmv28bCxh6+yXgzMV70VvvtwXfcdx3vo98PT+R8IH8o/2j5sfVT0Kf7kxmTk/8EA5jz/GMzLdsAADhoaVRYdFhNTDpjb20uYWRvYmUueG1wAAAAAAA8P3hwYWNrZXQgYmVnaW49Iu+7vyIgaWQ9Ilc1TTBNcENlaGlIenJlU3pOVGN6a2M5ZCI/Pgo8eDp4bXBtZXRhIHhtbG5zOng9ImFkb2JlOm5zOm1ldGEvIiB4OnhtcHRrPSJBZG9iZSBYTVAgQ29yZSA1LjYtYzEzOCA3OS4xNTk4MjQsIDIwMTYvMDkvMTQtMDE6MDk6MDEgICAgICAgICI+CiAgIDxyZGY6UkRGIHhtbG5zOnJkZj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYtc3ludGF4LW5zIyI+CiAgICAgIDxyZGY6RGVzY3JpcHRpb24gcmRmOmFib3V0PSIiCiAgICAgICAgICAgIHhtbG5zOnhtcD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZGM9Imh0dHA6Ly9wdXJsLm9yZy9kYy9lbGVtZW50cy8xLjEvIgogICAgICAgICAgICB4bWxuczpwaG90b3Nob3A9Imh0dHA6Ly9ucy5hZG9iZS5jb20vcGhvdG9zaG9wLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOnhtcE1NPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvbW0vIgogICAgICAgICAgICB4bWxuczpzdEV2dD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL3NUeXBlL1Jlc291cmNlRXZlbnQjIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iPgogICAgICAgICA8eG1wOkNyZWF0b3JUb29sPkFkb2JlIFBob3Rvc2hvcCBDQyAyMDE3IChXaW5kb3dzKTwveG1wOkNyZWF0b3JUb29sPgogICAgICAgICA8eG1wOkNyZWF0ZURhdGU+MjAxOS0xMS0wNFQxNDoyMjozOCswMjowMDwveG1wOkNyZWF0ZURhdGU+CiAgICAgICAgIDx4bXA6TW9kaWZ5RGF0ZT4yMDE5LTExLTIxVDEzOjM3OjQ3KzAyOjAwPC94bXA6TW9kaWZ5RGF0ZT4KICAgICAgICAgPHhtcDpNZXRhZGF0YURhdGU+MjAxOS0xMS0yMVQxMzozNzo0NyswMjowMDwveG1wOk1ldGFkYXRhRGF0ZT4KICAgICAgICAgPGRjOmZvcm1hdD5pbWFnZS9wbmc8L2RjOmZvcm1hdD4KICAgICAgICAgPHBob3Rvc2hvcDpDb2xvck1vZGU+MzwvcGhvdG9zaG9wOkNvbG9yTW9kZT4KICAgICAgICAgPHBob3Rvc2hvcDpJQ0NQcm9maWxlPnNSR0IgSUVDNjE5NjYtMi4xPC9waG90b3Nob3A6SUNDUHJvZmlsZT4KICAgICAgICAgPHhtcE1NOkluc3RhbmNlSUQ+eG1wLmlpZDo1NTA5NWNhYy00ZWEyLWIyNDgtYTY1ZC1lN2JkZjQ2ZTNjMjc8L3htcE1NOkluc3RhbmNlSUQ+CiAgICAgICAgIDx4bXBNTTpEb2N1bWVudElEPnhtcC5kaWQ6NTUwOTVjYWMtNGVhMi1iMjQ4LWE2NWQtZTdiZGY0NmUzYzI3PC94bXBNTTpEb2N1bWVudElEPgogICAgICAgICA8eG1wTU06T3JpZ2luYWxEb2N1bWVudElEPnhtcC5kaWQ6NTUwOTVjYWMtNGVhMi1iMjQ4LWE2NWQtZTdiZGY0NmUzYzI3PC94bXBNTTpPcmlnaW5hbERvY3VtZW50SUQ+CiAgICAgICAgIDx4bXBNTTpIaXN0b3J5PgogICAgICAgICAgICA8cmRmOlNlcT4KICAgICAgICAgICAgICAgPHJkZjpsaSByZGY6cGFyc2VUeXBlPSJSZXNvdXJjZSI+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDphY3Rpb24+Y3JlYXRlZDwvc3RFdnQ6YWN0aW9uPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6aW5zdGFuY2VJRD54bXAuaWlkOjU1MDk1Y2FjLTRlYTItYjI0OC1hNjVkLWU3YmRmNDZlM2MyNzwvc3RFdnQ6aW5zdGFuY2VJRD4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OndoZW4+MjAxOS0xMS0wNFQxNDoyMjozOCswMjowMDwvc3RFdnQ6d2hlbj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OnNvZnR3YXJlQWdlbnQ+QWRvYmUgUGhvdG9zaG9wIENDIDIwMTcgKFdpbmRvd3MpPC9zdEV2dDpzb2Z0d2FyZUFnZW50PgogICAgICAgICAgICAgICA8L3JkZjpsaT4KICAgICAgICAgICAgPC9yZGY6U2VxPgogICAgICAgICA8L3htcE1NOkhpc3Rvcnk+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjcyMDAwMC8xMDAwMDwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6WVJlc29sdXRpb24+NzIwMDAwLzEwMDAwPC90aWZmOllSZXNvbHV0aW9uPgogICAgICAgICA8dGlmZjpSZXNvbHV0aW9uVW5pdD4yPC90aWZmOlJlc29sdXRpb25Vbml0PgogICAgICAgICA8ZXhpZjpDb2xvclNwYWNlPjE8L2V4aWY6Q29sb3JTcGFjZT4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjE2PC9leGlmOlBpeGVsWERpbWVuc2lvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxZRGltZW5zaW9uPjE2PC9leGlmOlBpeGVsWURpbWVuc2lvbj4KICAgICAgPC9yZGY6RGVzY3JpcHRpb24+CiAgIDwvcmRmOlJERj4KPC94OnhtcG1ldGE+CiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgCjw/eHBhY2tldCBlbmQ9InciPz7kdukkAAAAIGNIUk0AAHolAACAgwAA+f8AAIDpAAB1MAAA6mAAADqYAAAXb5JfxUYAAACUSURBVHjarFPBDYAgDGyNi7gOhCH8uARvl+DjEASX8cEo56uGGESrkvRDctfrXcsA6NPTEsRsscFDsJ0GWG2sURCzBQAq65bg3LlUcEkwLybMiwlP/OvPQCIiP67T7ymUo0gSh4KWXD+uU8wWbkhc9ePtIomJndY8ATLztQJNCs0RhKS1C+oUXt+CGxLX/vnrOe8DANmHlbAQ0QQaAAAAAElFTkSuQmCC"
    },
    "FREEZING_GAZE": {
        "name": "Freezing gaze",
        "description": "A heart-freezingly sinister aura",
        "meta": {
            "action_type": 0,
            "action_max_uses": 20,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 45
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA4klEQVQ4jbWTIQ6DQBBF/9YhCJfoEUAhewQ8cAiOgWpSP9T3GFVFIhCQJjhSRRCYJluxUJaFhaZNx0yyO+9lZhaAf4Yd82CrZrcGWwZIldgxD+QzpoP3JqhoEaYRS5RuqM9hGrFkJlDhAbQMATYdQgAYxEyFXQd0vYkiANibAizaKTgbwY554Hug80UUug6ozvXgRCDDvgcqK0DuYikmIxzvnKsFZSVynYv8eI53TQdAXuLPHeh2AIyi1R3IEvUVXAeUZfPnWxQMkq+/A51kOOuvqc+hbqy35HDifOtfWI1PCl/dh53IqjJBHQAAAABJRU5ErkJggg=="
    },
    "GLOWING_BOLT": {
        "name": "Pinpoint of light",
        "description": "An extremely concentrated point of light that explodes after a moment",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "spread_degrees": 6,
            "action_mana_drain": 65
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABJElEQVQ4ja2Sv07DMBDGvxa2LExZkKzwApWiCOUBmDKFnT2JYEeZMucFivIC7VwW/AyV4oU5Q7DEcl6QKmYzgKP8cdMO/KZzzt93dzkvMEOWZWvf978ZY+E4J6Xcp2n6fDlOlGW5y/P8HgA8z7tOkuQGwMrifwUAEwMj7mETA8CKMfa17LdrYiLamFgpdUT/S2dQVdWTiV3XfQCAtm0/AWznDC7mknVdvx0Oh7swDG8dx5nkm6b5WFp0A+I4fgSOjzJrwDnXQRBACIGiKF6UUtue0buUcj/Zgk0cRdHi7/Pa87xXc6dt2+lcnHPNOddEpDnnenY+AMYZ/cuWyh1EtDFbGlQ1ENFZ1cuy3HUHPYKITrZusG5BCHFS2H+5gy7O+XH/yg/p6aFGEeI/rAAAAABJRU5ErkJggg=="
    },
    "SPORE_POD": {
        "name": "Prickly Spore Pod",
        "description": "Summons a spore pod that attaches to a surface and then grows and explodes into spikes",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAARFJREFUOI1jYBhowIhLovzWu/8MDAwMQQIsDOs+/GHoVBPCqharYPmtd/9vbziJIqYaYI7VECZcmssTrFHEb284yXDy1af/eF2AbPOPW+cYGBgYGBLLIxjmd65g4FAzwuoSuAtOvvqEoZmBgYFhfucKuFh5gjXD7Q0n4eEDdwG6zTBbsQF0l6CEAcxmmObE8ggUzeh8lDA4+erT/84FRxl+3DrH8DPaF6vt7Es3M3CoGTGoBpgzmDC+ZwhVVUaNlfJb7/47778Eif+u7f+NnRv+B3Vth/vXef+l/8j+xwuc91/6b+zcADcQF8BIBzDNH1rWMfAKyBO0CCNlIWtmzjFm2OuohzO54wRBXdsJOp1qAABDdYmTFMLCVgAAAABJRU5ErkJggg=="
    },
    "GLUE_SHOT": {
        "name": "Glue Ball",
        "description": "A projectile that explodes into a sticky mess",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "spread_degrees": 5,
            "action_mana_drain": 25
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA9klEQVQ4y6WTMaqDQBCGv0mq3CEnWERrb6KQJoXnkJzDwkZQyEGsXpG8sCfwDlYyr3Fl81hNQgaEGdn5+f9PV1SVb2rnmrop9ef3quM0LI+bQ+/rplQA8R3UTQmgcRJxvz0AJE4inXuyvOCwP0rQAcD5dAEQtwCoMSlZXgiAtf16hP8VJxFZXohb8lw9lYQgjtMAoF1bLRGcwPl0kU0H4zTQtdVi15jUdyUvI3Rt5Y8ayv0Wg/vtQZYXOIih/EEGcwTN8iIUQw7742uBNeuz0JPIbiufD/BjBv6PsyW0JiD+krU9xqRY28tbEP3P6J91jc9Avr3OfxPYn04PkmaGAAAAAElFTkSuQmCC"
    },
    "BOMB_HOLY": {
        "name": "Holy Bomb",
        "description": "Summons a bomb that... well...",
        "meta": {
            "action_type": 0,
            "action_max_uses": 2,
            "fire_rate_wait": 40,
            "reload_time": 80,
            "speed_multiplier": 1,
            "action_mana_drain": 300
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAvElEQVQ4jWNgwAP+n4j8///5zP/41DDhkyQGMOKyGUVA3gGiWDIdQz0LNgMuLz/AwMDAwKAbCdHI8PAAec67lC/5H8M1aACrFy7lS/7X9VNAddWmBwx6E58T9gI2zQxaCQy6WgwMlxga/qMbghILMM2XNz1A0QwDuhUNDJfyJVG8hGIATDOGC9DU4DQAK7i2ADsbCrBGI15D0ABWF6CEAQGAYgCyRnRDYHx0cYx4RQ9lGIAFMHo0UpyQKAYAkuFPkigIed0AAAAASUVORK5CYII="
    },
    "PROPANE_TANK": {
        "name": "Propane tank",
        "description": "Summons a propane tank. Be careful what you wish for.",
        "meta": {
            "action_type": 0,
            "action_max_uses": 10,
            "fire_rate_wait": 100,
            "speed_multiplier": 1,
            "action_mana_drain": 75
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAEOSURBVHjaYvz//z8DJQAggFiQOYyMjHB2e/k6uMmVnUFwCXQLAQKICZupCSFV/z++fw9mg2gQH5cLAAKIiYFCABBALNgE9eWPMTj4fAWzD2y5yCDOi9sAgABiwuZ8Bx99uGYQAPFxeQMggFjQNS9Y0wYMsKr/EkLKDPyC9uAwmDj1LgNIHGoII7IegACiOAwAAgjFgM8/XuFVjE0eIIAYkeMVlA6CfVL+83KIMUC8IAj2wot3d8Ga126Zw4ieDgACCMMLIEXYbMclDhBAeMMAlpjwAYAAYsKVDsIjLwHjfzGYBvFxAYAAwggDWHSCaFA4gPwPApDoxcwLAAHEhC+0YZrxxQ5AADFSmp0BAgwAgoBt32GHM9IAAAAASUVORK5CYII="
    },
    "BOMB_CART": {
        "name": "Bomb cart",
        "description": "Summons a self-propeled mine cart loaded with explosives",
        "meta": {
            "action_type": 0,
            "action_max_uses": 6,
            "fire_rate_wait": 60,
            "speed_multiplier": 1,
            "action_mana_drain": 75
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AYPCx0Khc58IQAAABR0RVh0RmlsZSBOYW1lAFVudGl0bGVkLTGwoMEPAAA542lUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgNS42LWMxNDggNzkuMTY0MDM2LCAyMDE5LzA4LzEzLTAxOjA2OjU3ICAgICAgICAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOnhtcE1NPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvbW0vIgogICAgICAgICAgICB4bWxuczpzdEV2dD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL3NUeXBlL1Jlc291cmNlRXZlbnQjIgogICAgICAgICAgICB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iCiAgICAgICAgICAgIHhtbG5zOnBob3Rvc2hvcD0iaHR0cDovL25zLmFkb2JlLmNvbS9waG90b3Nob3AvMS4wLyIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIj4KICAgICAgICAgPHhtcDpDcmVhdG9yVG9vbD5BZG9iZSBQaG90b3Nob3AgMjEuMCAoV2luZG93cyk8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgICAgPHhtcDpDcmVhdGVEYXRlPjIwMjAtMDYtMTVUMTQ6Mjk6MDkrMDM6MDA8L3htcDpDcmVhdGVEYXRlPgogICAgICAgICA8eG1wOk1ldGFkYXRhRGF0ZT4yMDIwLTA2LTE1VDE0OjI5OjA5KzAzOjAwPC94bXA6TWV0YWRhdGFEYXRlPgogICAgICAgICA8eG1wOk1vZGlmeURhdGU+MjAyMC0wNi0xNVQxNDoyOTowOSswMzowMDwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXBNTTpJbnN0YW5jZUlEPnhtcC5paWQ6NjhhNWZiNzUtNGM1ZC1iMjRhLWIwYmItZGFiY2UyYjAxNGU3PC94bXBNTTpJbnN0YW5jZUlEPgogICAgICAgICA8eG1wTU06RG9jdW1lbnRJRD5hZG9iZTpkb2NpZDpwaG90b3Nob3A6MzU4YTJkYTMtNDY4Ni0wYzQ4LTlhMDAtNmRkZTIwMjY4ZTQwPC94bXBNTTpEb2N1bWVudElEPgogICAgICAgICA8eG1wTU06T3JpZ2luYWxEb2N1bWVudElEPnhtcC5kaWQ6ZmNhNDk2NDktZTU1NC01MTQwLWE2ODgtOGQ3ZTIyYzhkNTAyPC94bXBNTTpPcmlnaW5hbERvY3VtZW50SUQ+CiAgICAgICAgIDx4bXBNTTpIaXN0b3J5PgogICAgICAgICAgICA8cmRmOlNlcT4KICAgICAgICAgICAgICAgPHJkZjpsaSByZGY6cGFyc2VUeXBlPSJSZXNvdXJjZSI+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDphY3Rpb24+Y3JlYXRlZDwvc3RFdnQ6YWN0aW9uPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6aW5zdGFuY2VJRD54bXAuaWlkOmZjYTQ5NjQ5LWU1NTQtNTE0MC1hNjg4LThkN2UyMmM4ZDUwMjwvc3RFdnQ6aW5zdGFuY2VJRD4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OndoZW4+MjAyMC0wNi0xNVQxNDoyOTowOSswMzowMDwvc3RFdnQ6d2hlbj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OnNvZnR3YXJlQWdlbnQ+QWRvYmUgUGhvdG9zaG9wIDIxLjAgKFdpbmRvd3MpPC9zdEV2dDpzb2Z0d2FyZUFnZW50PgogICAgICAgICAgICAgICA8L3JkZjpsaT4KICAgICAgICAgICAgICAgPHJkZjpsaSByZGY6cGFyc2VUeXBlPSJSZXNvdXJjZSI+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDphY3Rpb24+c2F2ZWQ8L3N0RXZ0OmFjdGlvbj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0Omluc3RhbmNlSUQ+eG1wLmlpZDo2OGE1ZmI3NS00YzVkLWIyNGEtYjBiYi1kYWJjZTJiMDE0ZTc8L3N0RXZ0Omluc3RhbmNlSUQ+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDp3aGVuPjIwMjAtMDYtMTVUMTQ6Mjk6MDkrMDM6MDA8L3N0RXZ0OndoZW4+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDpzb2Z0d2FyZUFnZW50PkFkb2JlIFBob3Rvc2hvcCAyMS4wIChXaW5kb3dzKTwvc3RFdnQ6c29mdHdhcmVBZ2VudD4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OmNoYW5nZWQ+Lzwvc3RFdnQ6Y2hhbmdlZD4KICAgICAgICAgICAgICAgPC9yZGY6bGk+CiAgICAgICAgICAgIDwvcmRmOlNlcT4KICAgICAgICAgPC94bXBNTTpIaXN0b3J5PgogICAgICAgICA8ZGM6Zm9ybWF0PmltYWdlL3BuZzwvZGM6Zm9ybWF0PgogICAgICAgICA8cGhvdG9zaG9wOkNvbG9yTW9kZT4zPC9waG90b3Nob3A6Q29sb3JNb2RlPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICAgICA8dGlmZjpYUmVzb2x1dGlvbj4yODM0NjUvMTAwMDA8L3RpZmY6WFJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOllSZXNvbHV0aW9uPjI4MzQ2NS8xMDAwMDwvdGlmZjpZUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6UmVzb2x1dGlvblVuaXQ+MzwvdGlmZjpSZXNvbHV0aW9uVW5pdD4KICAgICAgICAgPGV4aWY6Q29sb3JTcGFjZT42NTUzNTwvZXhpZjpDb2xvclNwYWNlPgogICAgICAgICA8ZXhpZjpQaXhlbFhEaW1lbnNpb24+MTY8L2V4aWY6UGl4ZWxYRGltZW5zaW9uPgogICAgICAgICA8ZXhpZjpQaXhlbFlEaW1lbnNpb24+MTY8L2V4aWY6UGl4ZWxZRGltZW5zaW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAKPD94cGFja2V0IGVuZD0idyI/PiLYU8sAAAAYdEVYdFNvZnR3YXJlAEFkb2JlIFBob3Rvc2hvcDTLjmcAAAAPdEVYdFdyaXRlcgBTdXBlclBOR8XEr90AAAEYSURBVDjLxZO/SsNQGMV/t5SQBAqpUOkgyZJBRx9GN918A9HJqZPgGzgIjs5OvkTHFtqhDQ4lgeRCIQlZroPe5GqvU4ee5R6+7zuHc/8JpRT7oMeeOLxBH+D24bk9CFlkBMPRvwKz/zS5ET2A+WyKLDKAHbGuawTDEbLImM+mXQLfH7DZfJKsFjiuR1NXv0TJagHQ9hzXw/cHnUFZbkVTVyo4OkbmKY7rWeM3dYWeGY9PhJlAnZ6d7wjKcotOqNcwigmjmGS9VIAQF1d36qcAgMm1iTawzfU0CaP4e78GN2ETh1HcvYO/QpmnyDylqStknlqNAPpamKyXmPzj/UWYCS6v761bFUqptqmTvL0+Ctst2ObEwX/jF1DWmwQOjXd3AAAAAElFTkSuQmCC"
    },
    "CURSED_ORB": {
        "name": "Cursed sphere",
        "description": "A projectile that brings bad luck to anyone it hits",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABMUlEQVQ4y9WTPW7CQBCFv0Xp3YBdULiBipaG9Q3s2jeI4AYcghtE5AbU5gaGhtYVNBQpkCjsC+SlYL1xSIpIpMmTVtqd2Tc/+2aNJB5BjwfxtwGaJOP+vIgiNUmmJsm0iKJv/RpJLKKI1WgKwGa/VT5LjfPL2QDIZykAQVm0fp4AVqOp3CXjFoA2+y22H3vi27Fidz2TJ5naIG0LXSLL00EAth8zHE8IyoKgLBiOJ9h+DIBvRxKSWBuoBrGqQUxtU60Nqm0qSapt+mXf8d0qaJKM53fRreLXcNnVVrE2aB6Gqm2qahD7zG321jYPQ0m6PSJgXns+uXGKaHc9YwGcvP4RxxNeLhfjZeyQyWepNvstTsofZQRMUBafc3A/QF0sTwe1M7I8HViNpp7sA/zvz/QBbmyysTlwU88AAAAASUVORK5CYII="
    },
    "EXPANDING_ORB": {
        "name": "Expanding Sphere",
        "description": "A slow projectile that increases its damage over time",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA90lEQVQ4y9WToY6DQBRFzyODIShWVFTUVaHxDXq/ANdf6HdU7EfwBTXrUDXVKNzKGhKSOiZ9K2BaBtbVbF8y5k3umTv3zYiq8koFvFj/GHDer/W8XytE9GWsEGlfxtqXMRCNC2Qa4inbKECSWgDJ8k7D4r6A92UAIGFxezoYxZKklrY2APwlnvcDJ64aS9VY15cs7/z5Xo9zjs4zENdcnH49wurgQdx+MCOKy2C851BOvDrMcxgAVWNltzWPzbY2+v31IZ7hiXjieADstsZBtK0NSWr5vPz4LpZT8Md4yjY4+yNEAbK8E0DD4v4Y3zOHm/8O3vMv/AL4nWcf+FmMkQAAAABJRU5ErkJggg=="
    },
    "CRUMBLING_EARTH": {
        "name": "Earthquake",
        "description": "Calls the anger of the earth",
        "meta": {
            "action_type": 0,
            "action_max_uses": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 240
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAuElEQVQ4jZ2SvQ3DIBCFD5QNXLhlBxgCicJzMBFzpEBiCNiBNkVmINVFZ35tP8kyYPPuewfMGVlCzPBE78+XcesT00o0H3trtY59KwwnzsgCABBiBq3E6T3TCwfWJ4ZGONYgyoqC0+rUiGoWp/mZEuB8FofXm0PMDZFWAkbNXhL0CgwJsGK9iRItI/QqYqxejJPBCH2mYVZ6L9D8Ug/qk7A+sdtks6Y5I8uxb//nkSE1uUVWGwAA/AAC64FDnlsMYgAAAABJRU5ErkJggg=="
    },
    "SUMMON_ROCK": {
        "name": "Rock",
        "description": "Create a mighty rock out of thin air",
        "meta": {
            "action_type": 0,
            "action_max_uses": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABF0lEQVQ4jc2RoW7DMBRFj6sSK5UCSr1NQQkI6G+MTd0PjA2uYKBkfKBgn7AvqMb2G50UkKCS0ESK1MjwDUy2nCwtnHaR9d67x8/X8NeSw1pERM4ObLY72Wx3kwNyWPu6g6ixeWx6e31WoUkppRxMrfbKN2/vHiRaRBiTAFDXRwD6U0+a5R4kIsLXPWq1VwBzB4gWEQDWdmgdeyNAVRZ+O7eB08wd+lMPQNO0WNuRZjlVWVDXRw+feuaMCYUQYxKMSajKwvcfn15kEuC2CCFOaZZjbYe1HU3T+vo8BFzfXKF1DPyE2DQtyyVoHftQxxcNNnBka7tBJmPz58e7D3KQqPtKNxiG54Ch+RdgCnTJfBYQQi6Z/4e+AaFdny3P8V03AAAAAElFTkSuQmCC"
    },
    "SUMMON_EGG": {
        "name": "Summon egg",
        "description": "Summons an egg that houses a friendly creature",
        "meta": {
            "action_type": 0,
            "action_max_uses": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA5klEQVQ4jc2QMQ6CQBBF/4iNdCQ2EmNHYaUVB8FDaOJlbEz0DuhBMDFYUVjYkN2GxA4rMjbuAtkVrYzT7GTm/zczC/w6OI2YmfkrcXyTHN+kFnMa1fkL0usy+64D33WgIbNDLbgs8BagzKKsAEBDiIiYmTmNmOZHAoC+DeC7TusVZaVzIqKm1thgczobH6TMtp4BCCdjAMD1/tA1lateJ0DdHXgDbQy8QavXCcgLaYWIskJeyM8A2ya2yW8BWZLoSaKstDkvJLIkMQBkVAAstzuehiHGw1HLvF+vDL0V0ISorWzm/4gnnFN+jTmkPewAAAAASUVORK5CYII="
    },
    "SUMMON_HOLLOW_EGG": {
        "name": "Summon hollow egg",
        "description": "Summons an otherwise empty egg that casts a spell upon cracking open",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -12,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABNklEQVQ4jc2Rv0rEQBDGv8gh+ARecR4naieXKzT4FgEF38AuvSFptFDs9QlstNHrrw0HwSoRu1yMeIf4p5SQBAKfxbFrYkJsLJxmZnbn++3MLPDX9nm+zKZ7erskWa1JIpsAkDg6RR4GPsPAZ1Es41qIozOJbCaOzjDwmaUxszSWkKJIwBTRdmuwA3Q2v2mzB+Sei8WDx3k6nWBtQ1VIEv4elMGt0tiBeP366lJ207Sb0g6yNGYS2SWI8D91CyJYWj1RACD3XBwfHeIl35dF9/5dyTeaOx4xS2NapsEw8GmZhszd8ah5DAAoziwg4ifqRmjVQT7e3wAAfVVDZ2Uds+lEnv0KmM+pAQC2t1QMhzfoddt4en6t3UH1LwFYpsG+qqHXbQOAFJ+eXVTqawFFiOiqTvw/7AsSHPG36pWeDwAAAABJRU5ErkJggg=="
    },
    "TNTBOX": {
        "name": "Summon Explosive Box",
        "description": "Summons a box of explosive matter",
        "meta": {
            "action_type": 0,
            "action_max_uses": 15,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAz0lEQVQ4jc2QMQrCUBBE35ekEqIgaIiQK6iNKB5AbBXMNeJJkivYCuoxBCs9ginETrERJLA25qOQELWQTPV2lxnYgX9LdmMREfnZrPkZYiSLje8LwDaK6LouaUxrycafST8MFfsJAKUkIDoeATicz5m88Dx6QYDsxqLaKwWgkoDpcCCOXf/olXC+1j7j9XC5XnPNVct6m/ULg3KDkVnjHl3oqEom929meoBtGLiOg21ZuZwacIpjvuFEBSpxUmnqskZmLZMzSzzFsS4rj4ulB5bFd/2Hw27GAAAAAElFTkSuQmCC"
    },
    "TNTBOX_BIG": {
        "name": "Summon Large Explosive Box",
        "description": "Summons a large box of explosive matter",
        "meta": {
            "action_type": 0,
            "action_max_uses": 15,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA9ElEQVQ4jc2SsUrDUBSGvxuSSUgLhRoi5BVqoUhKV0G6pmCHLvENevENfIEEX6B1FYwv4K64+QjeoWRLySJIIA5qajukXtqh3/RxD/ycy/lhRwTA83RaArwqxZnnUed+qKDzwIuU9ONYGABqsQDgPctq3Q8V1zLjfjzGj6LVBpcXg9J12lvXjeYJMgy+XQrEaSLM3+Eyz7cGXAXnzJIneBshoxIAA2BwdMzQavGplnRFo9YnvR63N9D/sFYBjmniuS6ObWt5FbALBxKQFkX1oOtaPdgkvnvU68FfmrYN/Hxh1Dip7j20Wv/ytR6kRVHdWMf3whedwoE3Qxd4gQAAAABJRU5ErkJggg=="
    },
    "SWARM_FLY": {
        "name": "Summon fly swarm",
        "description": "Summons five flies to aid you in battle",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 60,
            "reload_time": 20,
            "speed_multiplier": 1,
            "spread_degrees": 6,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABBElEQVQ4jWNgoDbwjMj4j028buH9/27527HKYQV1C+9jKK5beP8/ugWM6Iqb4hVRxGB8Iwe//+ISUgzbV8xgRDaABcYIspeDMKAaT5y7gWL7uQObUDTCABPMlnUHH8ENghuGwysYBiAbAjPIwkiDgYGBgeH09k6sAYsrsBmMHPz+e0Zk/IfR+46dg9PY1KP4C2bqyxfPGBgYGBjEJaRQFKMHIIoXwgu64DbANMI0YNMIAyzIHD4JJYbbF46gKIC5BkYTBEYOfv+NHPz+w9jIchce/P3PwIAaM0wMaABXfDMwMDCsO/iI4cKDv/+xxgy6bfhc6BmRgZGk4QCnBJIBxFpGFAAAluWClDSvBJoAAAAASUVORK5CYII="
    },
    "SWARM_FIREBUG": {
        "name": "Summon Firebug swarm",
        "description": "Summons four fire bugs to aid you in battle",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 60,
            "reload_time": 20,
            "speed_multiplier": 1,
            "spread_degrees": 12,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABFElEQVQ4jb2SvWoCQRSFz4hlQNg3sB0h1S5ELSxNLxtI5YJVniLY+Aq2SZoFrS3sYuEPKFi57YBVqoF9gptiucuscxdMkwPLzFzu+fZcZoD/0mY8p1k4oT8ZpNp02PUhm/Gc+OPaLJxUzpntieYmb9ovTwUMhWl7OWF7OZWNOtgpKWkDAAafb8osDiWIYXWjeAAXwqB+JwQAfP984Ta6e/ZiPfafafSQw9gc7aCFOFVYvhLiVIljVApMNjYvxglaleb39d4DlCO4sdjIBsnoAeK06DE2LxO4adxarTLboyTSlESaAIBX1nV1JqB6Mw23QQc79XHMauOaxQHX1Zmkm4H0R0lJpGk67PqvUiQK4jHv6b1Lv2JyiZnd5E30AAAAAElFTkSuQmCC"
    },
    "SWARM_WASP": {
        "name": "Summon Wasp swarm",
        "description": "Summon six wasps to aid you in battle",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 60,
            "reload_time": 20,
            "speed_multiplier": 1,
            "spread_degrees": 24,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABEklEQVQ4jWNgoBeoW3j/v1v+9v8kacAm9nBXCYo4I7ripnhFFDEYf0OP1//Fe98yrN1+khHZABYYI8heDsKAajxx7gaK7QEl21A0PtxV8l/erYcRLli38P5/uCEMDAxl/bsYGBgYGCyMNOCuwAaYkJ2+7uAjhnUHH8E1MjAwMGzutGX4+f7W/4e7Sv4basn8//n+Fv6A1LP2gCuG0ehsZPUoToMFVKyzMAMDAwODoZ4WhgXybj3YvRPsaf7/4a6S/w93lfwP9jRHsW1DjxcKjdUFwZ7m/xkYGOC2MzBghjxegM9mGLjw4O9/BgbsiQwF4HJy3cL7/y88+Psf5l0MjTBv4AMwzRhqsZqIw3XoUUkRAADN96NAQlVg0AAAAABJRU5ErkJggg=="
    },
    "FRIEND_FLY": {
        "name": "Summon Friendly fly",
        "description": "Summons a friendly fly that attacks your enemies!",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 80,
            "reload_time": 40,
            "speed_multiplier": 1,
            "spread_degrees": 24,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABrElEQVQ4jb2Tv0sbYRjHP5eYTqFBkCKUyFHoEBBRpHT0qHQ4nWrxVrnhpCC5zcEjo/QPuGw9pDjU4RU7uIp4XSRN8MgQaECUSCoqaFCUDIrGQe94vXS1z/K8P57v5/2+v+B/xtXyZkfOAImwITQrLzQrLwvkvtCsTnu9zNXyZpQBesLCmum4AEIC1EzHFYDhe0XD9xShWR1t/WEuPTOuPHEAMDU2QM103BBWCuoPkEcnhu8p/tk+r9YWlFDTE9/n1NgAAD9xXII6paDOoDRv+J4i1ycBVhtBee683frVO6Tn1AwAOTXDRmkPgBcTn/S583ZrWh19v9oIyl2AOOTPwQU5NcPldYq/R6ccVZeophN6NZ3Qv/aNtGRIUqaFkG+VFb1c2aay9YNM8oR5e5bt3zsAfHS+6JN7dxHkyX6EZuW/9yddgMNmA4DXWTV+TJjHt7bhe0WIvYNQLAvN41s7nkNxBBCalV8bfhuJD5uNyIHsRh4Lo+sa5aJCKmsv3jTdQiprG75XFJqFvHrkwPC94ufqrv2y/w25dx8opLJ211KPdf8aj0L+D/H8bHEP3lTMBu1/1+EAAAAASUVORK5CYII="
    },
    "ACIDSHOT": {
        "name": "Acid ball",
        "description": "A terrifying acidic projectile",
        "meta": {
            "action_type": 0,
            "action_max_uses": 20,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA6klEQVQ4jWNgoBAwYhOMrff5z8DAwKBu+A8uVhOwDataDMHYep//MI0e5hJw8R0nX2A1hAmXZnTgYS7B0LLB6z9eA2Dg5nkmuK2EAIYBN88zMagb/oM7H2bIjpMvGG6eZ8JwBYoBH19/gbMnzniF4hIYGzlcMAzgF+Vh+Pj6C1wxupfUDf8xTJzxCrcBMBfAaHXDfyiG3TzPhOJKBgYs0eiX5QD3I78oD4rcx9dfGDZNO4CiByMQkTUhu+jj6y8YBmI1ANmWTdMOMMIM2TTtACO2NIJhAEyzmRsXhhh64GIFsHyAi09/QMgFALIyb/CDzf7kAAAAAElFTkSuQmCC"
    },
    "THUNDERBALL": {
        "name": "Thunder charge",
        "description": "A projectile with immense stored electricity",
        "meta": {
            "action_type": 0,
            "action_max_uses": 3,
            "fire_rate_wait": 120,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAApUlEQVR4nGNgIA94TH2Lh4sd9J7+iocLN4KFGPt7T38tNuWGsJnQjIEYjOwkZNUoNhSbckNUozkGDTBi2s7AwFBkwtV35hvcIGQFTMgugYAiEy44ufvUD5w2eEx962rGAVfKwMCAZonH1Lc7soWxOwnuKohSiCCEjaIBbgkywO4HiDGuZhxwaQgD0w/oLoGw4fHQe/or/lBG14zJJayBAX9aJJhOAZfvXCOeYoPpAAAAAElFTkSuQmCC"
    },
    "FIREBOMB": {
        "name": "Firebomb",
        "description": "Slow, fiery bolt",
        "meta": {
            "action_type": 0,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAARUlEQVR4nGNgGAVQcFtX6FOhOrGqP683/v///////z+vNyas+lOh+n8kcFtXCE0BExr/5Z7XyFxxF1FqOwnuMEzHjGAAAM4vKis96tBtAAAAAElFTkSuQmCC"
    },
    "SOILBALL": {
        "name": "Chunk of soil",
        "description": "Don't soil yourself",
        "meta": {
            "action_type": 4,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAABQklEQVR4nK1SvWrDMBC+NKFwOBgJjEBgKAQHd8mQhk6ZsrSPkDFr9jxHHqJjH6Fd+gABd/BSgzEEAgJRsDExgoChwxnFTQ3t0Bs+0N33c0IC+JdaLafr9aJz1Ls4bzaPnwft+cLi0/N7t4CoyBkAmLwgNnLmONdtWY8W8HwRR/vJ3U2b5PkCANqB2+3LAACUNmmaBcHwJwk583xRVSelDfITAAwAoCprx+0rbaqyplwiEcbRXgqUAk1eNALH7VtZmh4dtw+gkbM42lMfQ5YliuyuyJLYhACQpkfysx0AmNz731aySJlKGykQQ2byIt4dgmCYJapJsDwqshyFUmmTJQo5C4Kh0mYUykZA3u2VqrLOEjUKZVXW8e6AnNlLnx9uPhvbNEIypunr20f315jPxm22FHjxNbrrYXG7Wk5/5/2lvgDJW8biCZ9LKAAAAABJRU5ErkJggg=="
    },
    "DEATH_CROSS": {
        "name": "Death cross",
        "description": "A deadly energy cross that explodes after a short time",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA7klEQVQ4jWNgGDTAsusLg2XXFxSxhM2f6xM2f/bCp44JxjhexsNwvIwHm9mmyBx0NUwMBIAA7+sGdDEcFmGCpQ+///////9/4lSjgYTNn+v/////f+69b38TNn+ux6WOEV0TAwPE2bpyEm+SFDlFGBgYGObd//4vSZGTKXHLlwYGBobTC3x5t2EYAA1tUwHe1w2mipIMUXIcKDbNu//93+VHL5g+fBb1ZmBgYEA2BK8Xlj78/h+fF/CCpQ+//y84cA9vIOKNxtP3nzMwMDDsQBfHmpDQUyEDAwPDh8+iDR8+i04m2wUMaCGOy6KBBQDI9GeC3MQRSAAAAABJRU5ErkJggg=="
    },
    "DEATH_CROSS_BIG": {
        "name": "Giga death cross",
        "description": "A giant, deadly energy cross that explodes after a short time",
        "meta": {
            "action_type": 0,
            "action_max_uses": 8,
            "fire_rate_wait": 70,
            "speed_multiplier": 1,
            "action_mana_drain": 150
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABC0lEQVQ4jaWTPW7CQBCFPyyEXKR2hbsULtKm8AWs9SUQKTgBUg4R4RNQBHwGJK8saqhoUiFaXFGnsNxsqknsjb2JxFfuvvdmZn/gTkYA8dtnZ/H4+gDAYqNmAOu5zkUne8LYTmwLpunkGXgCctmTYqLz2kY7HSAKknqoQCegjyhIHkNq5erS60sVQmp1xdeuIoMsNmpmjDEHszJymH2MRDxNJy9RkNQhtYpZciQjZvnTNhkAV3x9vpV+VTTv67nOv2+hKpoP0tIjSDRkyjbLKPvt7vLvEQ5mZYwxzhGcyBm4NB78folCu3Ub8Xj2Qpu/zJ2APvbb3eV8K/0hM1h/wX7nwKkqmpOry7v5Ak/mdf7OxFtKAAAAAElFTkSuQmCC"
    },
    "INFESTATION": {
        "name": "Infestation",
        "description": "A bunch of magical sparks that fly in every direction",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": -2,
            "speed_multiplier": 1,
            "spread_degrees": 25,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAQklEQVQ4jWNgoAW4MuPmWkJq/v///x+/ABmGEmfTgAOCXsGmgKAmXP6kmf8JGoxNAdnRSDQgK/AoAWQFMNGa6Jp8AUwYP2lt/NDQAAAAAElFTkSuQmCC"
    },
    "WALL_HORIZONTAL": {
        "name": "Horizontal barrier",
        "description": "A thin, horizontal barrier that harms passing creatures, including you",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAeUlEQVQ4jWNgGAUDDxiFJrrIMzAwMPBJC/vABAusDJmwKZ5w7Pw/GPvT07dbGBgYGFj4pIUfyCowMwRL6TE4i3ow7H29g8FZ1AO7dVYMcDVrn12a8vjBXwYWBgYGhscP/jKsZbiEohbdIBgfqpnh8YO/1PHCKBgMAACaDjJlI1ROvQAAAABJRU5ErkJggg=="
    },
    "WALL_VERTICAL": {
        "name": "Vertical barrier",
        "description": "A thin, vertical barrier that harms passing creatures, including you",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAuklEQVQ4jWNkwAGEJrrI80kLP2BgYGD49PStwrv8PQ+xqWPCZQAMyCow45XHaQCftLCPrAIzQ7CUHgOftLAPyQYwMDAwBEvpMTiLepDnAhjY+3oHXnkWXBIFVoZMcNutGJgKGFaS7gJCthM0gJD/CRpADKCtARSFwYRj5/85i3ow7H29g2HCsfP/yHYBoYDEmQ4YGBgY1j67hFczXhd8evp2y+MHfxnWPrvE8Onp2y0kGwADjx/8xSsPAK2PM5yaRItcAAAAAElFTkSuQmCC"
    },
    "WALL_SQUARE": {
        "name": "Square barrier",
        "description": "A thin, square-shaped barrier that harms passing creatures, including you",
        "meta": {
            "action_type": 1,
            "action_max_uses": 20,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABW0lEQVQ4jaWTMWvCUBSFPx9ZO1RJhy5xUKFuXRxKcUnBYNdQx/oDCnXs1NkxxT/QjlJXJYFmseLg4taCUTCLQ0NduhbskhdiMFTwLO8+7jnn3ct9Fw5ERgb5XqNeK2pNx/M3taKWkecukeP5z0uzO4gM8r1Gva4c9UuVAgC6amwJ3MBGVw3cwAZgNpkz+P25XprdgQCoFbVmqVKIhJIoETfUVYNSpcBxTvQBlHgiFPaAoRvYWOPpJUDr4nwU0qqAqasGn8H6dSoNHM/f3JWjR4at04dOGHcAWnQBsFZt3MA2ddXgTM2+A4hkn/8hyRGyxH3EsqV4LNLp+0EknfdBfEoiLbELchq6akSxkn260qQ4HGXVWrXTPKqSF1bdUdb3b/7jqj0CbsKkCZhpZcd/ZNSCNZ4uZpP51oh2/UYpnk3mAC+QWCbgVt6Ti+R4/qZ8kssAfHx9R8t0MP4A2TKVg9XkcAAAAAAASUVORK5CYII="
    },
    "TEMPORARY_WALL": {
        "name": "Summon Wall",
        "description": "Summons a shortlived obstacle",
        "meta": {
            "action_type": 6,
            "action_max_uses": 20,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA2klEQVQ4jaWSMQrCMBiF3y/dFIdCnIpIBnGOQy/QCzg4eEIHh17AC3Rx1g6lSCcLHaRjIQ410ZYmqfimF8L35U8I8GdIlZ1/OoaMrwEgKbM0rvaHIUBKKYlIc54qIePrKOBCrePKDXcEUcDFVjTvFRe4ueGOYLmYA6u67cVsFNwRsKACNtO255/5bTAATEwbY2CrQEopbaCKZ9pQJyuRaRLrFRRIRGSayCn4Fg1J9BXKwge71rqbJEbB/fEEyxvdTem/iRaci+wCtF+57eOiBUmZpUO9H9e/+DkvSZFazm8ufo4AAAAASUVORK5CYII="
    },
    "TEMPORARY_PLATFORM": {
        "name": "Summon Platform",
        "description": "Summons a shortlived bit of ground",
        "meta": {
            "action_type": 6,
            "action_max_uses": 20,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAoUlEQVQ4jWNgGHLgPxTA+Ey4JCiygRRxogwhy2UwTYQ0M+GTpBgQ43SKXcDIwMDAECi0erm5qJKai4ySkZwYH4OozDusil8/EWJ49OoTw54n986dfH3v1vp3oZEsDAwMDDDNxkZ/GBgUvjAwaHBjNUD0xhcG0Qd/GBgYlIwYGBgY1r+jghdYGBgYGE6+vncLwlUyknvCwyD6gDgvUGo5dQAAiJNZoUHYvzIAAAAASUVORK5CYII="
    },
    "PURPLE_EXPLOSION_FIELD": {
        "name": "Glittering field",
        "description": "Small explosions appear randomly over a large area",
        "meta": {
            "action_type": 1,
            "action_max_uses": 20,
            "fire_rate_wait": 10,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAY0lEQVQ4jb2TwQ3AIAwDWZLRsgNTsAbfzJAFzKtShagJENVv++RYSkp/qOVarsIA4IaYKEIbhMNmDV30lmsxUQCAic43oXTmYfQxQDcYzW/oqtkn3R0OF7t12eoZ9foHjsM76nOobAtNhmlMAAAAAElFTkSuQmCC"
    },
    "DELAYED_SPELL": {
        "name": "Delayed spellcast",
        "description": "A static, magical phenomenon that casts 3 extra spells after a short while",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAd0lEQVQ4jb2RsQ2AMAwEH2q2oQyTMEQGyAgZxD07mJIym7g3FZKLUGA5fGnp719+4G+pqo4zs9ARksxCyW12J7NQBoDaitZWfIm1FWWhxELZQlgoaUcAMHc4K4DLHrZlP6eOXlvY9C8/iJnQAH1TPmuENRkCCdUNQg1oOOMxPEQAAAAASUVORK5CYII="
    },
    "LONG_DISTANCE_CAST": {
        "name": "Long-distance cast",
        "description": "Casts a spell some distance away from the caster",
        "meta": {
            "action_type": 6,
            "fire_rate_wait": -5,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAW0lEQVQ4jWNgoDf4svX//y9b//+H8VlI1Tyrfw6czePNyMhEZQcS5wpkLxAN/v8nQxNVAEk2k+NMnLFArGGM2DQwMjIyYleOwwBsthFrCMUuQAkDUjRiNWBAAAAwizQMLxlX4gAAAABJRU5ErkJggg=="
    },
    "TELEPORT_CAST": {
        "name": "Teleporting cast",
        "description": "Casts a spell from the closest enemy",
        "meta": {
            "action_type": 6,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "spread_degrees": 24,
            "action_mana_drain": 100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAZ0lEQVQ4jWNgoBX4svX//y9b//8npI4Fl+ZZ/XPgbB5vRkZcBjCR7URCgFgvEAX+/6eSQRQDol1CipPJigW8FvxHAsRoZCSogJGR8f////8ZGbEnJgxBZENwaUIGWMOAGI14DaArAACPJ0P7WyQWXQAAAABJRU5ErkJggg=="
    },
    "SUPER_TELEPORT_CAST": {
        "name": "Warp cast",
        "description": "Makes a spell immediately jump a long distance, stopped by walls",
        "meta": {
            "action_type": 6,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "spread_degrees": -6,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAXklEQVQ4jWNgoCb4////f1L1MFFqKSM22xkZGRmxK8dhADanE2sIxS5ACQNkjTs9z5AWoDAXILuEZEOwgS9bSY9a2hhCTiKjjSEMDBR66cvW///7XGb/p9gQsjWjAwDV2D3oMFAEcQAAAABJRU5ErkJggg=="
    },
    "MIST_RADIOACTIVE": {
        "name": "Toxic mist",
        "description": "A cloud of toxic mist",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAwklEQVQ4jWNgoBaYdSr2/+YHFf9J1ccE03zn5GeG56+ewg2jmssIAUZCCmCukRSTZvBV6MBQz4RNAywsyiYHwDUzMDAwYAsjJnTN6Io/PPkHDxtkF8EAC7LEnZOfGRjMn8JtVDHnZbhz8jMDAwM3AwMDxJAPT/6huIARRTMUiEhzMwjIQByHLI4MVMx5GdLMFjNiBAq6Ydg0YbgAnyHYNJENsKVWjGjEp/nOyc8M1889RolOkpwGMwQWyCheo2v6RwYAkeFucIm57rQAAAAASUVORK5CYII="
    },
    "MIST_ALCOHOL": {
        "name": "mist of spirits",
        "description": "A cloud of potent alcohol",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA5ElEQVQ4jb2SIQvCQBiGnw3DwoQhhrWB3WRYFJPB6B8Q7BajWbBY/AX+AeOCyWwUu2BbOGQwwwVhBrnjdAdTEd90HN/z3st7H/xKu/mgOK6GxaecC7Ce9ov96YrIpTb7WbIqOVUDKk2z7tGebErzrg1QXSzGXQ0D2DpyXmE1DJAcBGHgETVq2kTkkt4s0ZxrwmaRAHHLJ80k58sNkUtE/jiXEihYSb0KYN6bils+vVnilEpZT/tFmkkbQxh4jJbbJ8b6C6aJDfpatm1921mlCgOPTuTrnfgommkSNWpP3/nf/Td1B2/7bEZPKLewAAAAAElFTkSuQmCC"
    },
    "MIST_SLIME": {
        "name": "Slime mist",
        "description": "A cloud of slimy mist",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA3UlEQVQ4jWNgoBaYErPy/7LMvf9J1cfEwMDA0OQ94/+jZw8Z3n1+AzeMai4jBBgJKYC5RohXhCFqujOGeiZsGmBhUebUBdfMwMDAgC2MGNE1wxQzMDAwXLh5loGHk49BSFAQbsi7z28YcpaEw/WxIGt+9OwhA4MUwkY5KXmGR88eotj47v17TBfANUMBzFYGBgYMA2BATkqeIWdJOCNGoDR5z/j/5fsnrJp4OPkY6rZmoOjBGgvIhmDTRDbAllqJNhnmKh5OPgYVOVV4miDJaciGCAkKokQnfdM/MgAAdOhh3OpGNgwAAAAASUVORK5CYII="
    },
    "MIST_BLOOD": {
        "name": "Blood mist",
        "description": "A cloud of blood mist",
        "meta": {
            "action_type": 0,
            "action_max_uses": 10,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAz0lEQVQ4jb2SMQqDMBiFn65CsovSwbmeICB0jbPX6NSDeA1nc4CCN+jsIHqBCJnTKWnUgLVI3xj+7/F4ecBZajnXr6rSR7kQAGrG9HMcMShlzU5Ltqdg78CkuUQRrk2zuQ99gOnikecWBgBfR+EaXh/3Utpu3EQbg5bzRZEAUKQppnm2JoNS6KVcJAhc2CghBBmlAAD33VWRpiiFCDal1IzpaZ69UEII7l23YLy/4Jr4oJ/lW+vXziZVQghucWw3cSiaa5JRilKID//X/bt6Azz+a5J12SBAAAAAAElFTkSuQmCC"
    },
    "CIRCLE_FIRE": {
        "name": "Circle of fire",
        "description": "An expanding circle of burning air",
        "meta": {
            "action_type": 4,
            "action_max_uses": 15,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABuklEQVQ4jbWTPWvbUBSGn1s8mahj0sZk0GAwXiLQ4NWbiL8gJgHHSwYPAo8duqR4CPZfCHjIkCUOxBCTOinZCp08BJQhXkTrwag0Xg1ZTwdLslR7KaUHhKSr+36c9+jC/6hhLiUy7cauYS4l6/a+ib60y3mRaVeK/RY8PiwW/Xux30KmXWmX82uJQrAMqvJsZ+TZzogMqjLMpUQG1fDbnyQqeJBpVwLF8ZcxE2eOs5kGwJi56IYGQHYvC6aF2rFV2ELYXwSsGxquSmLMXEojT02c+UpOAImgvxVlx+VidK+oFAS8cA3GZAPMjh0PEcDZTOOqJKWRpxr1mlzc3qvjSkGCNibOHExrzRRMi4kzx1VJDl+eaNRrcn55pRr1miQ23nK9tcvNz3eLLIIJxQgeH9ANjbS8cr21y/nllTquFARg//s30vKKMXMXIUYqAXB3cEqx3yIL4OdApSCHL0+Ubj31wc4Ijj8J3/7dwenSQWnkqSirbmh8fP8j/D+CqQROo5hEAOo0e5ycHS1cmJa/cYzOL9jWltZNi06zF4rFlNvlvJycHS2VQiJC651mj0+fv8ZwK/U3h+mf6zcGkvCWteOw2wAAAABJRU5ErkJggg=="
    },
    "CIRCLE_ACID": {
        "name": "Circle of acid",
        "description": "An expanding circle of dripping acid",
        "meta": {
            "action_type": 4,
            "action_max_uses": 4,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABn0lEQVQ4jaWTsWobQRRFzxiBB8nZyj+wBjfTRL0KGVK4WBUywo4axYXAH5DKxC4t0DcIVFhqHITJFt7SIBcLSedqGoHmB1ItK7FFYFLIO8lqQxq9ZuAx775779yBHUv8q/npybfNYFjovUTXTFqmdL/Q6N19sCc3VwDEhDRouzOv+WDE9PbZzVW2h2NCUq03IAp+Rd+JAxzY2wKbgziksf1q882p1kiTYF+PNjTrSzLfA+BAKRq06YuPAmAv17w9nPke64VE1JdMWkZIk5R8cgDNYFjaLE3C430kVjMF4Hqp1sSE5CbvbbtqX49YLySTlhG9ftc+3keicxnYXIY0ScFUB9CgjTQJ64Wkdq7p9bt2On4QvX7XVivvWM0U+98OyXyPmLAMEBOS+R7V44zVTDEdP4jOZWABxNkPqscZor7kQKkC4wpsQtIMhsQKUjQ1k9AhsLVzzaRlxMXn91aykXD6Rv8luqb0jH9nAHCG/u8ZXZDmgxEnN1fE6k9oUjT4PwHPUW/QZj4YFSUAeTxdlAFO1RdnWO78dpR3/kw7129OO8nAoNH7qgAAAABJRU5ErkJggg=="
    },
    "CIRCLE_OIL": {
        "name": "Circle of oil",
        "description": "An expanding circle of slick oil",
        "meta": {
            "action_type": 4,
            "action_max_uses": 15,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABlElEQVQ4jaWTMUscQRiGn1m8g2MKObL7Sa5S0lwhSwo7ET2Ra/IT/Csp8wPyJ65OlWYRVxE7i2QhcCAhVorfrUgCg+AVk2Kd0fVSBO6DYYePed/3m3fehSXL/Kv58cOaXx/ttnpX5Smfvt4unG81Dse5H42HAKjWiKTxG6ospkyKKuJWXoNVa/S6jgC9vAOIZE8CPpBEgqAcyxl+fHsiurxrkY7GQyZFBUAS7gzw89dtc9AZsJ4b1yWzhs9fvhtc+/oBkwCsj3ZRrXn4fQ/OMHMenOH4/MJU2gGIPb2uUa0JJq/wqmauUZ4UF+Zgb8cfnZyZ/e0tn9k52IbkpalJ2Iik4Aw3rksucwL4YG/HJ50elXaYKWA9qs9+xAlUa7Cet/aRSrscnzfKAJv9P8ycJ7MGGbxpTZxAExKRlN5qn0wglzn721s+lzlHJ2cG24CxHpEUkZSr8vSZYCFh1pNvPAJNPsKrxElfYKIHZTHl3cYavdU+m++HyCAF68mkIZRB2ixJKYtp1PqvKEeTWYzy0j/T0vUXHAvDlPAmmewAAAAASUVORK5CYII="
    },
    "CIRCLE_WATER": {
        "name": "Circle of water",
        "description": "An expanding circle of water",
        "meta": {
            "action_type": 4,
            "action_max_uses": 15,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAGHSURBVHjaYvz//z8DJQAggFiwCRo1n/lfHaiFIta6/hrDuVoTRnS1AAHEiOwC2eTF/ycUBoPZb/9wMQizfIPTMFDQv5bh8dxYuEEAAcSErhmkYf251wyHLj0Es2dsPgOmYYaC1IDUwvQBBBDcBWuvfPsPUwTSfO3ha4bXD66D5UQVNBm05EXBbDs9ebCLgnW4wK4ACCAmmJ/RNYM0mP9dCdYM8jtIDD2cQDRAAIEDERRg6DZfA4qfW7CFMSTBB6hwC1wM5gpYIAMEEBN6qIIUgmwG2RqXEvl/DdQQmDdAFiAHKkAAwQ0ACYIkQZrvKTcwgDQvmrMcbAgXCy+D0t0GhoMHD4ENgQUqCAAEENwAkCBI8iRzOFgxSDPE+QwMV+SLweIgV4CcjwwAAogJlkhALgBJ2tvbgV0A0gyiQQaBDIZ5AaQOhEF6QAAggFCiERaQMAALUHzRCBBAcAOQExIsBSIbBnM6SA45NQIEEFFJGaYRW1IGCCBGbLmRlMwEEECMlGZngAADANea7mRxYTS3AAAAAElFTkSuQmCC"
    },
    "MATERIAL_WATER": {
        "name": "Water",
        "description": "Transmute drops of water from nothing",
        "meta": {
            "action_type": 4,
            "fire_rate_wait": -15,
            "reload_time": -10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAiUlEQVR4nGNgGBggk/dSJu8lVikWTKVfHh6FsGPWfGZgYFgSwousgBFTNY+8NQMDg4MdF1wcWQ8TVtWY1mLRAHcJBBw49A2NgaIBqxcPHPoGUe1gxwVXwISpDqtOdBueTBLH6jC4IFwBwga4d9H0oIUElmBFM55H3hpuPLoGTD1oqnECPEmD9gAAi85Fgct2zr0AAAAASUVORK5CYII="
    },
    "MATERIAL_OIL": {
        "name": "Oil",
        "description": "Transmute drops of oil from nothing",
        "meta": {
            "action_type": 4,
            "fire_rate_wait": -15,
            "reload_time": -10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAm0lEQVR4nGNgGBgQF2cWF2eGVYoRU+mLF58kJPgYGBgUFIQYGBiamnbg1ICpGgKQ9TBhVY1pLRYNL158Qlb04ME7NAYEMMPN+PLlJwMDAw8PO1zuw4fvHz58hzhPXl7o4sWnKDbgAciWIDzt5qYBYWB648WLT7t23UD3A1wdmmfQQgJLsKIZLyHBt2jRKewaMPWgqcYJ8CQN2gMAQOJEUuoXmAAAAAAASUVORK5CYII="
    },
    "MATERIAL_BLOOD": {
        "name": "Blood",
        "description": "Blood blood blood",
        "meta": {
            "action_type": 4,
            "action_max_uses": 250,
            "fire_rate_wait": -15,
            "reload_time": -10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAA4KGlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgNS42LWMxMzIgNzkuMTU5Mjg0LCAyMDE2LzA0LzE5LTEzOjEzOjQwICAgICAgICAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIKICAgICAgICAgICAgeG1sbnM6cGhvdG9zaG9wPSJodHRwOi8vbnMuYWRvYmUuY29tL3Bob3Rvc2hvcC8xLjAvIgogICAgICAgICAgICB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIKICAgICAgICAgICAgeG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIj4KICAgICAgICAgPHhtcDpDcmVhdG9yVG9vbD5BZG9iZSBQaG90b3Nob3AgQ0MgMjAxNS41IChXaW5kb3dzKTwveG1wOkNyZWF0b3JUb29sPgogICAgICAgICA8eG1wOkNyZWF0ZURhdGU+MjAxNi0xMS0yMlQxNzoxOTowNyswMjowMDwveG1wOkNyZWF0ZURhdGU+CiAgICAgICAgIDx4bXA6TW9kaWZ5RGF0ZT4yMDE2LTExLTIyVDE3OjIwOjExKzAyOjAwPC94bXA6TW9kaWZ5RGF0ZT4KICAgICAgICAgPHhtcDpNZXRhZGF0YURhdGU+MjAxNi0xMS0yMlQxNzoyMDoxMSswMjowMDwveG1wOk1ldGFkYXRhRGF0ZT4KICAgICAgICAgPGRjOmZvcm1hdD5pbWFnZS9wbmc8L2RjOmZvcm1hdD4KICAgICAgICAgPHBob3Rvc2hvcDpDb2xvck1vZGU+MzwvcGhvdG9zaG9wOkNvbG9yTW9kZT4KICAgICAgICAgPHhtcE1NOkluc3RhbmNlSUQ+eG1wLmlpZDoyZTVkMGQ4Zi1jY2FlLTBmNDQtODJjZS00NGM0YzA3OGQxZTY8L3htcE1NOkluc3RhbmNlSUQ+CiAgICAgICAgIDx4bXBNTTpEb2N1bWVudElEPnhtcC5kaWQ6MmU1ZDBkOGYtY2NhZS0wZjQ0LTgyY2UtNDRjNGMwNzhkMWU2PC94bXBNTTpEb2N1bWVudElEPgogICAgICAgICA8eG1wTU06T3JpZ2luYWxEb2N1bWVudElEPnhtcC5kaWQ6MmU1ZDBkOGYtY2NhZS0wZjQ0LTgyY2UtNDRjNGMwNzhkMWU2PC94bXBNTTpPcmlnaW5hbERvY3VtZW50SUQ+CiAgICAgICAgIDx4bXBNTTpIaXN0b3J5PgogICAgICAgICAgICA8cmRmOlNlcT4KICAgICAgICAgICAgICAgPHJkZjpsaSByZGY6cGFyc2VUeXBlPSJSZXNvdXJjZSI+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDphY3Rpb24+Y3JlYXRlZDwvc3RFdnQ6YWN0aW9uPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6aW5zdGFuY2VJRD54bXAuaWlkOjJlNWQwZDhmLWNjYWUtMGY0NC04MmNlLTQ0YzRjMDc4ZDFlNjwvc3RFdnQ6aW5zdGFuY2VJRD4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OndoZW4+MjAxNi0xMS0yMlQxNzoxOTowNyswMjowMDwvc3RFdnQ6d2hlbj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OnNvZnR3YXJlQWdlbnQ+QWRvYmUgUGhvdG9zaG9wIENDIDIwMTUuNSAoV2luZG93cyk8L3N0RXZ0OnNvZnR3YXJlQWdlbnQ+CiAgICAgICAgICAgICAgIDwvcmRmOmxpPgogICAgICAgICAgICA8L3JkZjpTZXE+CiAgICAgICAgIDwveG1wTU06SGlzdG9yeT4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPHRpZmY6WFJlc29sdXRpb24+NzIwMDAwLzEwMDAwPC90aWZmOlhSZXNvbHV0aW9uPgogICAgICAgICA8dGlmZjpZUmVzb2x1dGlvbj43MjAwMDAvMTAwMDA8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDxleGlmOkNvbG9yU3BhY2U+NjU1MzU8L2V4aWY6Q29sb3JTcGFjZT4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjE2PC9leGlmOlBpeGVsWERpbWVuc2lvbj4KICAgICAgICAgPGV4aWY6UGl4ZWxZRGltZW5zaW9uPjE2PC9leGlmOlBpeGVsWURpbWVuc2lvbj4KICAgICAgPC9yZGY6RGVzY3JpcHRpb24+CiAgIDwvcmRmOlJERj4KPC94OnhtcG1ldGE+CiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgCjw/eHBhY2tldCBlbmQ9InciPz5c/tNYAAAAIGNIUk0AAHolAACAgwAA+f8AAIDoAABSCAABFVgAADqXAAAXb9daH5AAAAD6SURBVHjaxFMxbsMwDDxVdbx3MVBAXyj0hY59QbbkbdHWFxSe+gUhXzBQQBmyBahiCOx0jmRFU4ZyEQnyTkdSUiKCR+wJD9ozna/9vkqew7cAwMvwrta5j8OhJFgD5zABAJIHgnXSDeYuUdVCODr5HW9gbYFuMIWiJkE4OkkeSOgXcG5zmCqSguDqeyT00IgLCYFzmNANBpfxdF/B5D5lY+OSoE+S5IHLeMLGxkJFoYDF9BP6W0xlvjEDs9sq3kxgrkIjLjPJt1EqyPrOt7CeUXOIZrdVLOB59WVMpc13wAJK5kY0YgVuvkQW/jgn2gKvbzWQpv79N/4NAHkPez8HF2KXAAAAAElFTkSuQmCC"
    },
    "MATERIAL_ACID": {
        "name": "Acid",
        "description": "Transmute drops of acid from nothing",
        "meta": {
            "action_type": 4,
            "fire_rate_wait": -15,
            "reload_time": -10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAoklEQVQ4jWNgGNTAfb3Jf/f1Jv/xqWHBpfHK6Y9w/uyvMf8ZGBgYUrmXMKKrxRCAadYx5WdgYGBgCHHTQJFHN4QJn2ZcrsNpALKzYWDNrhtY2RgG4AusNbtuwDWHuGmgqGXCpQmfYcgAJUCkq1ThJuMKhyunPzI8bbsN14fiAmRN2MIDWwDjjEZsQMeUn2Fn4BkUPRgG4DIEm2aCgJikPPAAAEjBTZxio25YAAAAAElFTkSuQmCC"
    },
    "MATERIAL_CEMENT": {
        "name": "Cement",
        "description": "Transmute drops of wet cement from nothing",
        "meta": {
            "action_type": 4,
            "fire_rate_wait": -15,
            "reload_time": -10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAEbSURBVHjaYvz//z8DJQAggJgYKAQAAcQCYyxatAhD8v79+2DnKSoqMqLLxcXFgWmAAGLBZipI46dPn8BskBcvXrz4n4+PD6tBAAGE4QWQ4o8fP8I1MzIyMoA0I7sIGQAEEBO6ZpAmGAZpRgYgV6EbAhBAKF749+8fmAZphMUOiA3zDsglHz58QDEUIIDgLjh//vx/JiaEg2BsmEEgGqQZJI7sCoAAQvECcpqAeQOZj+wyGAAIILgBhoaGjDCbYYqQXQHSDAsT5NgACCAWXC7AZissjJABQACheAHkCmQ/I2uC8WEuhQGAAMJIBzAFMCfDXACi0TWDAEAAYU2JMIUXLlz4D9JoYGDAiCsvAAQQI6W5ESCAKM6NAAEGAJkvjfk8Vs6TAAAAAElFTkSuQmCC"
    },
    "TELEPORT_PROJECTILE": {
        "name": "Teleport bolt",
        "description": "A magical bolt that moves you wherever it ends up flying",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "spread_degrees": -2,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAoklEQVR4nL1PuxXCMBDTeQIX9ghehT5bwESwRXpWuRHswhscBTFPJCZ8/B7qpHfSScA/YWYbzY2Gyq+fRWTbQFUn5jnns5k9jgFYM7fQtxPI3NWl99l7f2AeQjh2AgQAnKpOKaWZzTHGE5tlVUNExJYNjs0AUGu9Mi+lXF5MuDdYJjR9Zs5N2MRwqoqU0pO45ntw3xzvotWmOZ81GH08HDCMG4dPOJ4Zl8MrAAAAAElFTkSuQmCC"
    },
    "TELEPORT_PROJECTILE_SHORT": {
        "name": "Small Teleport Bolt",
        "description": "A shortlived magical bolt that moves you wherever it ends up flying",
        "meta": {
            "action_type": 0,
            "speed_multiplier": 1,
            "spread_degrees": -2,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAbklEQVQ4je2QzQ2AIAxGX52AA6uYsJTLuF5HYIh6UIxAMTFeeRfSJnw/hYmLqiaAnPNmZtgJAOUtLM7nV3ERqWfPOYSwAsQYd0dA2l0XW1WTmdlV4eaqUHVoEwAMUwzdR0mejq1zoTviV34LTOAAoXBLUE2bbDMAAAAASUVORK5CYII="
    },
    "TELEPORT_PROJECTILE_STATIC": {
        "name": "Return",
        "description": "After a period of time, you'll be returned to where you cast this spell",
        "meta": {
            "action_type": 0,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "spread_degrees": -2,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAlUlEQVQ4ja2R2w3FIAxDY3bJQJ20A2WY9KOYm4b0pV5LVSHEBwMi/5aZLbnm7h7/Ue0KRgMA9BIyiAvlzqq6dsMEJ7PRzOZoJhTA9I100RRTZOCZxh2YGYdrmk+Kd8AjiKoemjhnr7uL7xIAYL3F5kq/B6jr9epF5ADASHBnRorRjzC/wtMUEXibIO+ca68AlT4DPmsDHN5XooO4+H4AAAAASUVORK5CYII="
    },
    "SWAPPER_PROJECTILE": {
        "name": "Swapper",
        "description": "It was theorized that the source of qualia would be transferred ΓÇªBut it turns out it was the whole body all along.",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "spread_degrees": -2,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAA7mGlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgNS42LWMxMzggNzkuMTU5ODI0LCAyMDE2LzA5LzE0LTAxOjA5OjAxICAgICAgICAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIKICAgICAgICAgICAgeG1sbnM6cGhvdG9zaG9wPSJodHRwOi8vbnMuYWRvYmUuY29tL3Bob3Rvc2hvcC8xLjAvIgogICAgICAgICAgICB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIKICAgICAgICAgICAgeG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIj4KICAgICAgICAgPHhtcDpDcmVhdG9yVG9vbD5BZG9iZSBQaG90b3Nob3AgQ0MgMjAxNyAoV2luZG93cyk8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgICAgPHhtcDpDcmVhdGVEYXRlPjIwMjAtMDQtMTdUMTM6Mjg6NDcrMDM6MDA8L3htcDpDcmVhdGVEYXRlPgogICAgICAgICA8eG1wOk1vZGlmeURhdGU+MjAyMC0wNC0xN1QxMzo1MjoyNyswMzowMDwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXA6TWV0YWRhdGFEYXRlPjIwMjAtMDQtMTdUMTM6NTI6MjcrMDM6MDA8L3htcDpNZXRhZGF0YURhdGU+CiAgICAgICAgIDxkYzpmb3JtYXQ+aW1hZ2UvcG5nPC9kYzpmb3JtYXQ+CiAgICAgICAgIDxwaG90b3Nob3A6Q29sb3JNb2RlPjM8L3Bob3Rvc2hvcDpDb2xvck1vZGU+CiAgICAgICAgIDx4bXBNTTpJbnN0YW5jZUlEPnhtcC5paWQ6YWJjYzFlZmYtM2QwZC1iYTQ0LWE1ZWEtODNjZjgzYmY3OWE2PC94bXBNTTpJbnN0YW5jZUlEPgogICAgICAgICA8eG1wTU06RG9jdW1lbnRJRD54bXAuZGlkOjU3NjliZmI1LTRiNGMtNDc0MS05MmY4LThkM2QxNDlhYmUwMzwveG1wTU06RG9jdW1lbnRJRD4KICAgICAgICAgPHhtcE1NOk9yaWdpbmFsRG9jdW1lbnRJRD54bXAuZGlkOjU3NjliZmI1LTRiNGMtNDc0MS05MmY4LThkM2QxNDlhYmUwMzwveG1wTU06T3JpZ2luYWxEb2N1bWVudElEPgogICAgICAgICA8eG1wTU06SGlzdG9yeT4KICAgICAgICAgICAgPHJkZjpTZXE+CiAgICAgICAgICAgICAgIDxyZGY6bGkgcmRmOnBhcnNlVHlwZT0iUmVzb3VyY2UiPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6YWN0aW9uPmNyZWF0ZWQ8L3N0RXZ0OmFjdGlvbj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0Omluc3RhbmNlSUQ+eG1wLmlpZDo1NzY5YmZiNS00YjRjLTQ3NDEtOTJmOC04ZDNkMTQ5YWJlMDM8L3N0RXZ0Omluc3RhbmNlSUQ+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDp3aGVuPjIwMjAtMDQtMTdUMTM6Mjg6NDcrMDM6MDA8L3N0RXZ0OndoZW4+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDpzb2Z0d2FyZUFnZW50PkFkb2JlIFBob3Rvc2hvcCBDQyAyMDE3IChXaW5kb3dzKTwvc3RFdnQ6c29mdHdhcmVBZ2VudD4KICAgICAgICAgICAgICAgPC9yZGY6bGk+CiAgICAgICAgICAgICAgIDxyZGY6bGkgcmRmOnBhcnNlVHlwZT0iUmVzb3VyY2UiPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6YWN0aW9uPnNhdmVkPC9zdEV2dDphY3Rpb24+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDppbnN0YW5jZUlEPnhtcC5paWQ6MmQ5YzRjNmItZTZhYS02NzQ5LWI4YzgtZGU2MDg1YWQzNmMxPC9zdEV2dDppbnN0YW5jZUlEPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6d2hlbj4yMDIwLTA0LTE3VDEzOjQ3OjIwKzAzOjAwPC9zdEV2dDp3aGVuPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6c29mdHdhcmVBZ2VudD5BZG9iZSBQaG90b3Nob3AgQ0MgMjAxNyAoV2luZG93cyk8L3N0RXZ0OnNvZnR3YXJlQWdlbnQ+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDpjaGFuZ2VkPi88L3N0RXZ0OmNoYW5nZWQ+CiAgICAgICAgICAgICAgIDwvcmRmOmxpPgogICAgICAgICAgICAgICA8cmRmOmxpIHJkZjpwYXJzZVR5cGU9IlJlc291cmNlIj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OmFjdGlvbj5zYXZlZDwvc3RFdnQ6YWN0aW9uPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6aW5zdGFuY2VJRD54bXAuaWlkOmFiY2MxZWZmLTNkMGQtYmE0NC1hNWVhLTgzY2Y4M2JmNzlhNjwvc3RFdnQ6aW5zdGFuY2VJRD4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OndoZW4+MjAyMC0wNC0xN1QxMzo1MjoyNyswMzowMDwvc3RFdnQ6d2hlbj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OnNvZnR3YXJlQWdlbnQ+QWRvYmUgUGhvdG9zaG9wIENDIDIwMTcgKFdpbmRvd3MpPC9zdEV2dDpzb2Z0d2FyZUFnZW50PgogICAgICAgICAgICAgICAgICA8c3RFdnQ6Y2hhbmdlZD4vPC9zdEV2dDpjaGFuZ2VkPgogICAgICAgICAgICAgICA8L3JkZjpsaT4KICAgICAgICAgICAgPC9yZGY6U2VxPgogICAgICAgICA8L3htcE1NOkhpc3Rvcnk+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjcyMDAwMC8xMDAwMDwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6WVJlc29sdXRpb24+NzIwMDAwLzEwMDAwPC90aWZmOllSZXNvbHV0aW9uPgogICAgICAgICA8dGlmZjpSZXNvbHV0aW9uVW5pdD4yPC90aWZmOlJlc29sdXRpb25Vbml0PgogICAgICAgICA8ZXhpZjpDb2xvclNwYWNlPjY1NTM1PC9leGlmOkNvbG9yU3BhY2U+CiAgICAgICAgIDxleGlmOlBpeGVsWERpbWVuc2lvbj4xNjwvZXhpZjpQaXhlbFhEaW1lbnNpb24+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj4xNjwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgIAo8P3hwYWNrZXQgZW5kPSJ3Ij8+MAqaAgAAACBjSFJNAAB6JQAAgIMAAPn/AACA6QAAdTAAAOpgAAA6mAAAF2+SX8VGAAAAqklEQVR42qRTMQ4DIQyzUbd+qA/qzGOY2/9U/Qqs19UdCiLHAVeJLBFxHIITKAkr5rBolzbweG0CQABD/3k/6b0fdsA//bDAmSiaFrjfrpyxW/ygQQgBAFDeOMIPHZAsRNnzKW73QBL0s11shjsAiDEiR1Vza1Yh9nBmL5K0JHNud8GOWw4AUkpd5VmFGE2GbkLCQMz5GHcbk19UOFa8EpsWaG/v5qx+5+8A4vdpKj7V1o8AAAAASUVORK5CYII="
    },
    "TELEPORT_PROJECTILE_CLOSER": {
        "name": "Homebringer Teleport Bolt",
        "description": "Brings the target hit closer to you",
        "meta": {
            "action_type": 0,
            "speed_multiplier": 1,
            "spread_degrees": -2,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAvUlEQVQ4ja2SwQ3CMBAEd6O8qcJpALkXCoEyQh/QS0QDvgb40sDy8UWW5cQgmE+UkzO3vhzwTyRBUpS0vvcYWkUzmwGAZFcwFt1jlSaSXFpJSjHLwymlue4wTdOlGwMAUkqzpJifcllLuokLXAIA9+f+JMdWMYRwMzO45IR1DgJAkpAEku2/AACPQzjvdfZBDlVxAQAzW44vuzKTk/hVpE8WxPHDKijr/U2pRDnp+t3mDFqwsZpfCVr8LPiZNwU8f0Y+h2yIAAAAAElFTkSuQmCC"
    },
    "NUKE": {
        "name": "Nuke",
        "description": "Take cover!",
        "meta": {
            "action_type": 0,
            "action_max_uses": 1,
            "fire_rate_wait": 20,
            "reload_time": 600,
            "speed_multiplier": 0.75,
            "action_mana_drain": 200
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAA7lWlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgNS42LWMwNjcgNzkuMTU3NzQ3LCAyMDE1LzAzLzMwLTIzOjQwOjQyICAgICAgICAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIKICAgICAgICAgICAgeG1sbnM6cGhvdG9zaG9wPSJodHRwOi8vbnMuYWRvYmUuY29tL3Bob3Rvc2hvcC8xLjAvIgogICAgICAgICAgICB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIKICAgICAgICAgICAgeG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIj4KICAgICAgICAgPHhtcDpDcmVhdG9yVG9vbD5BZG9iZSBQaG90b3Nob3AgQ0MgMjAxNSAoV2luZG93cyk8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgICAgPHhtcDpDcmVhdGVEYXRlPjIwMTUtMTEtMjRUMTI6Mjg6MjArMDI6MDA8L3htcDpDcmVhdGVEYXRlPgogICAgICAgICA8eG1wOk1vZGlmeURhdGU+MjAxNS0xMS0zMFQxNjoyMTo1MSswMjowMDwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXA6TWV0YWRhdGFEYXRlPjIwMTUtMTEtMzBUMTY6MjE6NTErMDI6MDA8L3htcDpNZXRhZGF0YURhdGU+CiAgICAgICAgIDxkYzpmb3JtYXQ+aW1hZ2UvcG5nPC9kYzpmb3JtYXQ+CiAgICAgICAgIDxwaG90b3Nob3A6Q29sb3JNb2RlPjM8L3Bob3Rvc2hvcDpDb2xvck1vZGU+CiAgICAgICAgIDx4bXBNTTpJbnN0YW5jZUlEPnhtcC5paWQ6OTc4OWVmNzctOTZjOC1jZDQ0LWJiODUtZDQxNGVkMzFhZTJkPC94bXBNTTpJbnN0YW5jZUlEPgogICAgICAgICA8eG1wTU06RG9jdW1lbnRJRD54bXAuZGlkOjFkM2FkOTkyLTVhYmQtZTg0NS05NjJjLWJiM2I3YWRkYmVkNzwveG1wTU06RG9jdW1lbnRJRD4KICAgICAgICAgPHhtcE1NOk9yaWdpbmFsRG9jdW1lbnRJRD54bXAuZGlkOjFkM2FkOTkyLTVhYmQtZTg0NS05NjJjLWJiM2I3YWRkYmVkNzwveG1wTU06T3JpZ2luYWxEb2N1bWVudElEPgogICAgICAgICA8eG1wTU06SGlzdG9yeT4KICAgICAgICAgICAgPHJkZjpTZXE+CiAgICAgICAgICAgICAgIDxyZGY6bGkgcmRmOnBhcnNlVHlwZT0iUmVzb3VyY2UiPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6YWN0aW9uPmNyZWF0ZWQ8L3N0RXZ0OmFjdGlvbj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0Omluc3RhbmNlSUQ+eG1wLmlpZDoxZDNhZDk5Mi01YWJkLWU4NDUtOTYyYy1iYjNiN2FkZGJlZDc8L3N0RXZ0Omluc3RhbmNlSUQ+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDp3aGVuPjIwMTUtMTEtMjRUMTI6Mjg6MjArMDI6MDA8L3N0RXZ0OndoZW4+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDpzb2Z0d2FyZUFnZW50PkFkb2JlIFBob3Rvc2hvcCBDQyAyMDE1IChXaW5kb3dzKTwvc3RFdnQ6c29mdHdhcmVBZ2VudD4KICAgICAgICAgICAgICAgPC9yZGY6bGk+CiAgICAgICAgICAgICAgIDxyZGY6bGkgcmRmOnBhcnNlVHlwZT0iUmVzb3VyY2UiPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6YWN0aW9uPnNhdmVkPC9zdEV2dDphY3Rpb24+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDppbnN0YW5jZUlEPnhtcC5paWQ6NDJjYTFjMTItMWJhNS03ZjRkLTliZDQtODVhZTM0MDNiOGE4PC9zdEV2dDppbnN0YW5jZUlEPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6d2hlbj4yMDE1LTExLTMwVDE2OjA5KzAyOjAwPC9zdEV2dDp3aGVuPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6c29mdHdhcmVBZ2VudD5BZG9iZSBQaG90b3Nob3AgQ0MgMjAxNSAoV2luZG93cyk8L3N0RXZ0OnNvZnR3YXJlQWdlbnQ+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDpjaGFuZ2VkPi88L3N0RXZ0OmNoYW5nZWQ+CiAgICAgICAgICAgICAgIDwvcmRmOmxpPgogICAgICAgICAgICAgICA8cmRmOmxpIHJkZjpwYXJzZVR5cGU9IlJlc291cmNlIj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OmFjdGlvbj5zYXZlZDwvc3RFdnQ6YWN0aW9uPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6aW5zdGFuY2VJRD54bXAuaWlkOjk3ODllZjc3LTk2YzgtY2Q0NC1iYjg1LWQ0MTRlZDMxYWUyZDwvc3RFdnQ6aW5zdGFuY2VJRD4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OndoZW4+MjAxNS0xMS0zMFQxNjoyMTo1MSswMjowMDwvc3RFdnQ6d2hlbj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OnNvZnR3YXJlQWdlbnQ+QWRvYmUgUGhvdG9zaG9wIENDIDIwMTUgKFdpbmRvd3MpPC9zdEV2dDpzb2Z0d2FyZUFnZW50PgogICAgICAgICAgICAgICAgICA8c3RFdnQ6Y2hhbmdlZD4vPC9zdEV2dDpjaGFuZ2VkPgogICAgICAgICAgICAgICA8L3JkZjpsaT4KICAgICAgICAgICAgPC9yZGY6U2VxPgogICAgICAgICA8L3htcE1NOkhpc3Rvcnk+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjcyMDAwMC8xMDAwMDwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6WVJlc29sdXRpb24+NzIwMDAwLzEwMDAwPC90aWZmOllSZXNvbHV0aW9uPgogICAgICAgICA8dGlmZjpSZXNvbHV0aW9uVW5pdD4yPC90aWZmOlJlc29sdXRpb25Vbml0PgogICAgICAgICA8ZXhpZjpDb2xvclNwYWNlPjY1NTM1PC9leGlmOkNvbG9yU3BhY2U+CiAgICAgICAgIDxleGlmOlBpeGVsWERpbWVuc2lvbj4xNjwvZXhpZjpQaXhlbFhEaW1lbnNpb24+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj4xNjwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgIAo8P3hwYWNrZXQgZW5kPSJ3Ij8+OEHeYwAAACBjSFJNAAB6JQAAgIMAAPn/AACA6QAAdTAAAOpgAAA6mAAAF2+SX8VGAAAA6UlEQVR42sSToQ7CMBRFTwkWNbUEMYVB48gMfAGCzBEkwaGGmJpgCgXhFxa+YQgWHD8xQYIBhcMUQWg2uiUbEzzXpvf29rxXIaWkTjWoWU2A81bItmkAYI5uooqB+DwhbVLFSKQZnLdCLdqmUcpEGQRBIAEsy9IOxXGMN9zjR2Ns2wbAcRyRgei6buFtH7E33Ku9MAylBnHSyQqPzw2AEr8T5HQB4HK961cbeoLj09YNenMp0hy+WfjRmGl3hx/NVIIkSfIh5tWgtWR9clj0Qw6PVYZZYRvTyUqN8i/iTBeqCnMn8S+/8TUAHQRpqUA8zgAAAAAASUVORK5CYII="
    },
    "FIREWORK": {
        "name": "Fireworks!",
        "description": "A fiery, explosive projectile",
        "meta": {
            "action_type": 0,
            "action_max_uses": 25,
            "fire_rate_wait": 60,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAArklEQVQ4jWNgoAXIOK3wn1i1TNg06Zv44jTsf+Yr3IZP/5/7Hwam/8/9j80l/5d/wm3AfzSAbvP/5Z8gEss//cdwScZphf/oLsBqCT4XwLwBMxCrAfjCAF0TKbGBFyAb9P///0KsikKWQQINRuNS/P///2ScNh1/9B8z2pAMwWYgPCGFLPv/31KOkRHZBVDwCQcbP0CzORmZJtkQJAO00NUwoQuggZ1Q+jhJNpMCAHAwnJ4hO52mAAAAAElFTkSuQmCC"
    },
    "SUMMON_WANDGHOST": {
        "name": "Summon Taikasauva",
        "description": "Summons a possessed wand to aid you",
        "meta": {
            "action_type": 6,
            "action_max_uses": 1,
            "speed_multiplier": 1,
            "action_mana_drain": 300
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAoklEQVQ4jWNgoDX4///ff7I16xdKkaa54NKhbYQ0EzQUZkj9dvH/yBr0C6X+9x358v///3//cRpScOnQtsPf312Caa7fLv4fhmGaYRjDEJhmsl1w+Pu7S8T4Fas4zHaiFKMBJhhj7e0rT9AlL/Y/Y+w78gWvIXADglV1ZLApKLLhYSTkCgYGBtQ0QApgQuZgCweSLEKPSooAzBCYq6hiKDYAAFoSiTpXpXeFAAAAAElFTkSuQmCC"
    },
    "TOUCH_GOLD": {
        "name": "Touch of Gold",
        "description": "Transmutes everything in a short radius into gold, including walls, creatures... and you",
        "meta": {
            "action_type": 4,
            "action_max_uses": 1,
            "speed_multiplier": 1,
            "action_mana_drain": 300
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAWUlEQVQ4jWNgoBAw4pL4fyflMopClTm6RJn4/07KZXTN+MTx2kqSGmI0Y1PLRKwmok1EF8cnh9cFyBrxeY9iL+A0ADneiUoDAxsLpLiCqNRISlKmfmYiFQAAzG5NdamWzjkAAAAASUVORK5CYII="
    },
    "TOUCH_WATER": {
        "name": "Touch of Water",
        "description": "Transmutes everything in a short radius into water, including walls, creatures... and you",
        "meta": {
            "action_type": 4,
            "action_max_uses": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 280
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAbElEQVQ4jWNgoBAw4pIwq3n2GJl/qkVKligTzWqePUbXjE8cr60kqSFGMza1LKQahDUskDXhMwCnOnRNuAIRG58Jn3OJBoRsJMZFeL2BTw4jFmDgVIuULEwhSakQnU9MAGMYQkpSpn5mIhUAADS8YPI33lt+AAAAAElFTkSuQmCC"
    },
    "TOUCH_OIL": {
        "name": "Touch of Oil",
        "description": "Transmutes everything in a short radius into oil, including walls, creatures... and you",
        "meta": {
            "action_type": 4,
            "action_max_uses": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 260
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAa0lEQVQ4jWNgoBAw4pKYUBjwGJlf0L9BligTJxQGPEbXjE8cr63YNOE0BJ9CfHJMeJ1ELMDlLHz+homz4DOYmJDH8AJZsQCTwBVw2NgwmgnmVIJxjGYhhveQTSbkBaLTAllqSA1E6mcmUgEAwUZlkZQWrSMAAAAASUVORK5CYII="
    },
    "TOUCH_ALCOHOL": {
        "name": "Touch of Spirits",
        "description": "Transmutes everything in a short radius into alcohol, including walls, creatures... and you",
        "meta": {
            "action_type": 4,
            "action_max_uses": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 240
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAb0lEQVQ4jWNgoBAw4pK4MtHrMTJfJ3+bLFEmXpno9RhdMz5xvLaSpIYYzdjUMmGTJNkLMAlkBYTYMBrDBaQCFmSOTv42WVzOhImjRyeGC5AVoPsbb1pAtxlXGKDzKQ4Dgq7AZztOQ0hJB9TPTKQCAB0/ZStt2IXoAAAAAElFTkSuQmCC"
    },
    "TOUCH_BLOOD": {
        "name": "Touch of Blood",
        "description": "Transmutes everything in a short radius into blood, including walls, creatures... and you",
        "meta": {
            "action_type": 4,
            "action_max_uses": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 270
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAZ0lEQVQ4jWNgoBAw4pLYaGp6A5nvf/q0BlEmbjQ1vYGuGZ84XltJUkOMZmxqmcjRRLbtyOpgbBZSDdpoanoDI0YocQHeMECOOqw2E3IFvjQBY7OgSyIDolMfPleQrIbUpEz9zEQqAAAdkkiwHRjIcAAAAABJRU5ErkJggg=="
    },
    "TOUCH_SMOKE": {
        "name": "Touch of Smoke",
        "description": "Transmutes everything in a short radius into smoke, including walls, creatures... and you",
        "meta": {
            "action_type": 4,
            "action_max_uses": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 230
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAY0lEQVQ4jWNgoBAw4pLIrFz2GJk/vT1KligTMyuXPUbXjE8cr60kqSFGMza1TNgkSfYCTAJZASE2jMZwAVkA3QZivIDXBdjinGA6wOVnQvIsuFxAVkqkKCEhKyAlHVA/M5EKAOxvYwJDCaW+AAAAAElFTkSuQmCC"
    },
    "DESTRUCTION": {
        "name": "Destruction",
        "description": "Instantly decimates foes around you, at the cost of your HP",
        "meta": {
            "action_type": 1,
            "action_max_uses": 5,
            "fire_rate_wait": 100,
            "reload_time": 300,
            "speed_multiplier": 1,
            "action_mana_drain": 600
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAyElEQVQ4jZVS2w3CMAy8IrZoBUPkiz9mqboEDNApEKuUP74YolIYoxw/uHUc98FJleLmzvY5Bn7o20iS7NtI+VeHZjwLRz4BLGEuiRVKvFlMkkPnCK1YLr32bQEA2Mvh80CGrJJCHRreX7ciIQ9d6k9i3X4dGkp3tstsUBqeTQAo+jbyeK0K23J1KrE7T/bi8w0AOFzKJIbOOgedONsD692bgfj37GZZl/Zg6VVgiVv3YMRaNWvVFWuiHZz3hK7YEr2Eq/7/ufsCrFDdmYuQMzcAAAAASUVORK5CYII="
    },
    "BURST_2": {
        "name": "Double spell",
        "description": "Simultaneously casts 2 spells",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAiklEQVR4nMWR0Q0EIQhE2e3BPiyC6cjEGqwJi7APmrgPEpcF7/Ny84UKvnEk+rNEJOxcfqGqpZRjNwAr7t2qqlZ8Q/XeiegSkVqrPzNINmOQG8BaKxjLhDnny1KQ0b3GGM9AhrTW9pVBZ0LQjugZyBBmPkLOBD8cxl4ft6MEYO9mZu8nKmef4/q9PtkxQWerYM6bAAAAAElFTkSuQmCC"
    },
    "BURST_3": {
        "name": "Triple spell",
        "description": "Simultaneously casts 3 spells",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 2
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAoUlEQVQ4jdWSzRFDERSFD5MW7BVDBaqw14smqIAObPShCFlJ/GVenpksclZc957zMYC/krW2zjUyF0oplTFGUkpLs3MOnHMAgNaaAADtB0spta2dc1/R0DZw4yaDKADknJcDYwyuKF4GJ4oxvg2klOQuhfd+fMST9MHgE4UQYqFo6ZcEO8M+HQAe/UZKSUIIdW4WQmzTt+oNZimljv/L7/QECkZPbiKaMnEAAAAASUVORK5CYII="
    },
    "BURST_4": {
        "name": "Quadruple spell",
        "description": "Simultaneously casts 4 spells",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAw0lEQVQ4jdWRsRGDMAxFHxyDUDACGwATwBL0FIxAwShmAjSEB/AGbtjAaYLjBAOXLnmVT2dJz/7w9ySfBWutm+eZrut8bZom6roGQERYlsX3pWGjtdYBDMOAUiq6cds2qqpybds6gGxv/ka7LEt/TgG01odLVxYh6e2NE4wxrwFN0yQxi51xHBERlFJv+rcGZ88wxvgk/IA7izMuDbTWPv/YdnjGGFqs6+ojFRGAw5CQLFbcfzjPc/q+TwBXFMVh+2/wABHBWROkn1n4AAAAAElFTkSuQmCC"
    },
    "BURST_8": {
        "name": "Octuple spell",
        "description": "Simultaneously cast 8 spells",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABPUlEQVQ4jaWSMa7CMBBEnxFCQqKgo3VBkZLCV8gJuAw34RLOCUhPAYpSoRQRoqIxAhdGSDSm+Y7iJPyGadar9c7ujA0/QnwrHA4HH85VVYn7/c7j8eD5fHI+n8myDIBRt9EYgzHGSymRUnI8HkmSxF+vV79YLHxRFN5a69frtQcYDwz37WS5XPYuKKWac2+DsiyjfLVaAVDX9aDUHkGapqJLYq1ls9mQ53kzPRBGEtrGaa0H1w/N0+lUNATGGGhp11qjlEJrjZSSPM97ZK/XK5Lwr3HW2ib+rS6iZ/xmXBdKKd7vd/R3BMB+v8c559uNZVkyn8/RWkfasyyLCEYAzrnBiaEpxGBcjwBgNptFxaF3D8a1MQZI05Tdbhd5cblcGjO7xrXRW2m73XoA55y43W4e4HQ6iclkwhDBz/gAQ7Oiq96uiAsAAAAASUVORK5CYII="
    },
    "BURST_X": {
        "name": "Myriad Spell",
        "description": "Simultaneously casts as many spells as you have left uncast in your wand",
        "meta": {
            "action_type": 3,
            "action_max_uses": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA4klEQVQ4jc1Su63CQBCctV4LrsJkUIKvBYgIIYXA0tXg7L0UKjAt3JZgJAcmpoArYgisQ/7csyCClU7a7+3MaIGvN+cc5+oyTnjvmaapxIaNMZP+pD/ovWfw/0NjrR3UEuccYwPjRgBQVZRlOUCRGGOkaZrJtqIooKrPOM9zbO9dzNOBJDmgMGeqittmhayqwdOB2P3itllN+QUd+s9ay8C9XS9Jku16+aT3MoLtXZFVNXA+Iqtq9D95CUV/81saBIFFRBaXqwCA7P9ERDo/hiLABjr1YwcU7CeWHA3MnvLn7QEPaqlkmRSKfgAAAABJRU5ErkJggg=="
    },
    "SCATTER_2": {
        "name": "Double scatter spell",
        "description": "Simultaneously casts 2 spells with low accuracy",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "spread_degrees": 10,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAL0lEQVQ4jWNgGDng/////78f9v9Plsb///8PBY0MDAwMZPmRqi4Y/AZRJYyGIQAAYX5A4fHqKk0AAAAASUVORK5CYII="
    },
    "SCATTER_3": {
        "name": "Triple scatter spell",
        "description": "Simultaneously casts 3 spells with low accuracy",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "spread_degrees": 20,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAMElEQVQ4jWNgoAf4ftj/P9ma////P6qZFIAe2t8P+/9HBwRjhCIXjBqCG1CUF2gOAHglRvFdTMPKAAAAAElFTkSuQmCC"
    },
    "SCATTER_4": {
        "name": "Quadruple scatter spell",
        "description": "Simultaneously casts 4 spells with low accuracy",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "spread_degrees": 40,
            "action_mana_drain": 2
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAR0lEQVQ4jWNgoDX4ftj/P9ma////P8g0/////z9BL2HT/B8KCNqMbjrRGrEZRLZGil1AURjgMwQmTnTCoigdUNUQivICXQAAeO9g7VpGYNEAAAAASUVORK5CYII="
    },
    "I_SHAPE": {
        "name": "Formation - behind your back",
        "description": "Casts two spells: one ahead of and one behind the caster",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAJklEQVR4nGNgGAU0Av///////z8yF00WWQ0TphwjIyNW7iigIQAA0PAX8QO5QrQAAAAASUVORK5CYII="
    },
    "Y_SHAPE": {
        "name": "Formation - bifurcated",
        "description": "Casts 2 spells in a bifurcated pattern",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 2
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAALklEQVR4nGNgoAX4////////IWwmgkpJMHXIKKUXINNJQ0gbCWmJkZGRHHuQAQAVfCnegqjAUwAAAABJRU5ErkJggg=="
    },
    "T_SHAPE": {
        "name": "Formation - above and below",
        "description": "Casts 3 spells - ahead, above and below the caster",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 3
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAMklEQVR4nGNgIA/8////////xKhkItMGkgGR7hkogOw8OBt7MGJVik8DLQHtrRtMaQkAkjgp25WMEB8AAAAASUVORK5CYII="
    },
    "W_SHAPE": {
        "name": "Formation - trifurcated",
        "description": "Casts 3 spells in a trifurcated pattern",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 3
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAN0lEQVR4nGNgoAX4////////IWwmgkpJMHXIKMWjE42NbigepVg0kOmkIaSNhLTEyMhIjj3IAAA8h0TDze9gDQAAAABJRU5ErkJggg=="
    },
    "CIRCLE_SHAPE": {
        "name": "Formation - hexagon",
        "description": "Casts 6 spells in a hexagonal pattern",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 6
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAO0lEQVR4nGNgoAT8////////+EWYKLKBTAB3A5pj6OwGiO1Y3QCXgjBQQomRkRFTA1ZBOgNaBuvgSEsAeDI11yS8AH8AAAAASUVORK5CYII="
    },
    "PENTAGRAM_SHAPE": {
        "name": "Formation - pentagon",
        "description": "Casts 5 spells in a pentagonal pattern",
        "meta": {
            "action_type": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAOUlEQVR4nGNgoAr4////////sUoxUccGkgEu9yCkCahAUkOZH7DaQ9DyQQfQXEw9D+CPCmRZ2qclANzhLNpMXg2HAAAAAElFTkSuQmCC"
    },
    "SPREAD_REDUCE": {
        "name": "Reduce spread",
        "description": "Reduces the spread of a spell",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "spread_degrees": -60,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAPUlEQVR4nGNgGBTg//9/////g7CZqGkqnMQJvh/2J6wIWSmmDeS4Cg2Q62kynUSCpzEBUcGKRzMNYppqAADp0y73/ZvW5QAAAABJRU5ErkJggg=="
    },
    "HEAVY_SPREAD": {
        "name": "Heavy spread",
        "description": "Gives a projectile a much lower cast delay, but no respect to your aim",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -7,
            "reload_time": -15,
            "speed_multiplier": 1,
            "spread_degrees": 720,
            "action_mana_drain": 2
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAUklEQVQ4jWNgGCjw//////gFaGXT7t27d+HVgE8BQc3YFMLYRGsmSyPMn7t3795Fsk2kuAhmOMEYo8gl+AIQr8GE4pvsWMAFyEq5RGuiWb7ABgDCaWQPuml7/QAAAABJRU5ErkJggg=="
    },
    "RECHARGE": {
        "name": "Reduce recharge time",
        "description": "Reduces the time between spellcasts",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -10,
            "reload_time": -20,
            "speed_multiplier": 1,
            "action_mana_drain": 12
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAz0lEQVQ4jbWSyxHDIAxE16nAITePW1BdasA1qSG14OEGdgfkBCPziSeZyR4R+5BWAP/SEULKOkJIPxlr9UBTbZ6dwxkjdu+hqgAAIsK6LMi15+s11aDy8hFCUtXEzFuuMfOmqsneaQC5zdo8glyKqlrIPXMNabroHg4kIiVUAHjcGUY6Y0QBqCrOGDE7h94ItjMiAgDs3l8vfcohh8fM23BUEbmsyUIsPG+qO5eF2JdscHebgoiket8WUI/QfkkDIiIQ0SQiTYjrsvS/9Ld6A2YjR1+Mnj4KAAAAAElFTkSuQmCC"
    },
    "LIFETIME": {
        "name": "Increase lifetime",
        "description": "Increases the lifetime of a spell",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 13,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABYklEQVQ4ja2TMWvCUBSFP8Wl4CBUirQdOmSw2MFB6aDdMnYoCP0LRXTo0KFD59JRiuLi4BYpCP4Ah0LiZAaHShwcHEKRaiGDkPF1sAl5hlBKe5b77uO8c8+93Ad/RGz3olQbC2dpksoU8OKoVfR5uuWKi9M9P4/vCizMTmSuW67Ipjfolit+tJartCXS9dOnAFitViKYh1rIVdoilSn4ubM0mfarkS1IKNXGQtM0oVuuODq/EbrlCk3TRKk29t14lT3Eg4/r5TkA2fRGiguzgyfycr8fCwnkKm3/saqqkqu7xisA9fI8NBeABEAqU+A4f+ZX9JC/fACg120AkDLeQm0nvIM9GWAHHBwcnvDxvqDXbWBPBt8sJVrgOH8lOXh+vAW2c8iqKrN1EqIcOEsTezKXHATFhsMhW54TEogDTPvVWNNQJPJubBqKtA+SAMCoVfRFZuukFJuGIv2HIH69if+OL0gno5IFYi8sAAAAAElFTkSuQmCC"
    },
    "LIFETIME_DOWN": {
        "name": "Reduce lifetime",
        "description": "Reduces the lifetime of a spell",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -15,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABU0lEQVQ4jbWTMUgCYRiGHyXB7YIchJwKnGoSJbhNEJpFh8aWJhGnhmjMNURKmtoPpVkQbvsXzUkXnW840OW2A4evwe7nzrMoom/5eP/73vd/v5f/4I+V2D2YmDVRGw8zZRD0ohroOafakNzbk8bJXYGeu/gWH99c/cxap1SRMHaqDQFYrVYSxrEVOqWKmClDY7XxaI1HCQAZKlkX8mSmSxKXZozLxKzJzLJEhkquT89FhkpmliUTs6bdyFBFnCXD5HSzDsC6kI/0nrsgENm9ORnYDsjZcjni6vHuAYB0sx7LBeAAwEwZnBk5fWNQtxdbsdfnFwD8UDaxFeaeg2vb+sPJ4ZEmzz2HuefEyNoBEHNw324D2xyybJN//0pAbTzSngO2ozMIi7m2jfs5t3eF1niU8Lt9Pbyv+92+fg97MyiqgRbJTJeR7nf7kf8hXL96if9SH3K6n4u6WRAxAAAAAElFTkSuQmCC"
    },
    "NOLLA": {
        "name": "Nolla",
        "description": "The duration of a projectile is set to zero",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAArElEQVQ4ja2TwRHDIAwEj1TjFpKfO3EJFMTD1ZCPe9p8REKwxDgzuRkegFikE0j/ELACtZsvQOGjAiwzQLXACmzAwVkHsF2BNFUg2+j3fIjd/D7s7O8hxGpuadcLkOPLk86wOnoyKbX0i03ZM3YA5BYsSTffUiml9JD0lHT3MhnTKt5tXiZRCb2J+wzimmhBfRtnkHMbA0j0kNxSR0j4lKPujJDpZzLIGgJ+0Qt90ND6tnaD8QAAAABJRU5ErkJggg=="
    },
    "SLOW_BUT_STEADY": {
        "name": "Slow But Steady",
        "description": "The reload time of the wand is set to exactly 1.5 seconds",
        "meta": {
            "action_type": 2,
            "reload_time": 90,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABKUlEQVQ4jaWTMY6DMBBFX1ablgOYxrKoOEBOgChTpvcNOAAUkW9Bw0WiHCAHSGWNqDgAomeLlVEcwiq7+5uRNePnP2Mb/qndVsI5N4sIxhhCbJpmVf+xBRCRaH29Xv9m0Vo7/5TfbCFsNMYsjrque68F59xcliVVVSEiHI9HyrLEObdyswI45+Ysy8jzHIC+71FKAd9zeIZEAGtttFkphdYagLZt0VqTZVk0l89HgDGGw+FAkiQMw7A4OJ1OaK2p6xoA7z0vASLC7XYDoCgKANI0Zb/fU1XVknvUpoOg8/kMwDRNFEXBOI6/czBNE0opkiThcrmsHKxuIdBD8f1+j9be++iVRoCu63YiskDGcYyi9371H6IWgpqm2Vlr5wB6PvXVi3xLr/7FF5tLktjg4L9lAAAAAElFTkSuQmCC"
    },
    "EXPLOSION_REMOVE": {
        "name": "Remove Explosion",
        "description": "Makes a projectile no longer explode",
        "meta": {
            "action_type": 2,
            "explosion_radius": -30,
            "fire_rate_wait": -15,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABCElEQVQ4jc1SMW7DMAy8EF4Koy/oTvgRKbJk9xuSAh3duS/QbI8Zkjd0z1I4Q34QIOha+AFGoSUQO7gUZEdOgGYJF1FH8UgeBdyNSZZJySxD3Na5x1qTSskskmUeI3Uq51AQISRpTSrhud48oSBC5ZwvkKjzdjxOwCwFEcAsy8V3r5PyL1Y5170dM22xZBZb52IbIyHWmlRsnYt2dcbUmlSGrar/+rkAvvY47bZ4fP+Z9AiUEQCS6RyrlwMKIq9PLLknYghesmQ6722GYo+0euWc385qtsFptwUAPDx//F9EoPsb4cjx5MZ0ijcmSnK1slawjRGd+SLJWMATDUhGNRjOFt6jc99qv1jf1H34WMH3AAAAAElFTkSuQmCC"
    },
    "EXPLOSION_TINY": {
        "name": "Concentrated Explosion",
        "description": "Limits the radius of a projectile's explosion heavily",
        "meta": {
            "action_type": 2,
            "explosion_radius": -30,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAtUlEQVQ4jWNgGAhwclLwf6IV9yboVp+cFPz//4tzKJpIMgQG0A0hC5BkyPfD/v8ZGBgYPrdzo2hCNoQJm0ZkDd8P+/9nsXGBG8bAwMBwalUrXC0LNgN4K78yMjAwMDCoWCAE75yAM4+cu1WD1QW9CbrV6M7HBooXXIY7AcUAGyO1Fqw67pxg4LTdyIjLQDhAD+Hvh/3/w8IDn4vwGkIWIMsQ9KT5/8W5/ycnBf9HDliSDaELAAAV4Fs0xhTqDwAAAABJRU5ErkJggg=="
    },
    "LASER_EMITTER_WIDER": {
        "name": "Plasma Beam Enhancer",
        "description": "Makes plasma beam spell's beam wider",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAeklEQVQ4jWNgoCdoPfD/P7oYI7Ga//9HaGZkZCRaH4rNMEOwuYSBgYGBwajx6n+jxqvYJfFphGnGxiYEGGEaztVro/jLqPHq/2B7LZwa1x68xgDXg9dpBAATuRphgAXmHHRAkhdgGrCxSQIURSM+gC8h0Scpo7uEqgAAzIlV2Vc2NbgAAAAASUVORK5CYII="
    },
    "MANA_REDUCE": {
        "name": "Add mana",
        "description": "Adds 30 mana to the wand",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": -30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAlElEQVR4nK1SUQqAIAwdkofwFp5JSKgDJRh0Jm/RIQzWx3BZmSX0fhxvb89tCvALlAnKhHfd4OPgY5055diedHxJoYYpkiIiIub2HHSXsqk/mBRHZyWTgg5nZWVKZUJeAyTlTu6gfkgmnlzfQcsp2ufdCh53XfST17ro8paclQAR0n7GeUtkCW0Px4mGr5Hj6+f7gh2Oz4snNWkVfAAAAABJRU5ErkJggg=="
    },
    "BLOOD_MAGIC": {
        "name": "Blood magic",
        "description": "Reduces a spell's mana cost and recharge time greatly, at the costs of four health points",
        "meta": {
            "action_type": 6,
            "fire_rate_wait": -20,
            "reload_time": -20,
            "speed_multiplier": 1,
            "action_mana_drain": -100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABEklEQVQ4jdWRsUrDUBSGP4uji0MWQRBcChcs5AkCYh8g481iO3TzBQqBQiEv0K1DKkIDLpmdpFkcCxUy6iLIhVCFrC3cDmpIehvRTf/pnHPP///nngP/Hnu7imkQ6ONut8hfwhAA0e/v7DfIuVI6V0pfCK+Ic6V0GgQ6kVL/mPz2vDJEcqUqIo26sQHCwZ1h8j4e87hYmO4jIXQiZcVp2z2RUidS6o7jT794laV0HH96efQkAU6aTQ57PeBjiQfrNYP7FXYWc5WmBW+/LDCZDT0cHzuL5fXrKfbtOXPLBcDOYrDcCJBlToMtTGZD76zVAogelsuiPrfcqPT2PW7a7dpTjYSoP+Ovmj5hfAGgPPrfxwZgiKMm5h3ZAAAAAABJRU5ErkJggg=="
    },
    "MONEY_MAGIC": {
        "name": "Gold to Power",
        "description": "Spends 5% of your current gold and adds damage to a projectile proportional to the amount spent",
        "meta": {
            "action_type": 6,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA0klEQVQ4jc1Tuw3DIBQ8Itf2Fu7SOWm8hJsoyjpewN4CWTRegoaUdAzg1KZNQRo/hL+xnCYnIcHj3r3jAcAA19UOR+G62rn3y/0ssig8jL7N/X44Z98ErRI+lhRyxmdEJmJSSBZWIBiukT7Ofn5pLPMCfZs7wzWyqvQV4+ttJGCVGMWsEkgKySIipwF5mryFiNQM18g2Eg3Xvkh4hEUHU1glkFWlX6cA0EgAwCl0cAS7HKzZnzmgzoZ3T/apsXSVhNWH9LzHLiRPKy9i71/4r4/3AdMmlSdUznclAAAAAElFTkSuQmCC"
    },
    "DUPLICATE": {
        "name": "Spell duplication",
        "description": "Duplicates every spell cast before it",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 20,
            "reload_time": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 250
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAa0lEQVQ4jWNgGFTgy73/cHBh9qv/ZGsmyxBkxRdmv4IbQpIrkAGxBjARY9iXewjvXJj96v+Xe0S4DNkFMC8h0wRtRFdIsmZkZxLtAmyaYeIEwwCXZkIAIxa4FREBSExCghtwZ+9rUiweRAAAHj/L02S0XpMAAAAASUVORK5CYII="
    },
    "QUANTUM_SPLIT": {
        "name": "Quantum Split",
        "description": "Makes a projectile split into three projectiles whose existences are entangled",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABB0lEQVQ4y6WTy03EQAyGP484bAGTpSJEExFHmkghK66pAikdQCWLpwBO+TlkJkxePC2NYiWx/T88Jon/xE1JzOzbn69QptktIInw00m52PLRNb8PvykO7tLlUhoJwIoGexTKtzezuUGhMDaNzpKFvYK1LsEdKgpj05T8U8S98Gmi5enkQoK7jU1zrIG7K6NZ8DpLs4iFTgBIKcndVZ5VYeGkGCMpJWU6mz1YK1hbBmApJdouAmjosfHdkHSoga0a0XZRQz/ldw9o6GUbGyVhZngFs0CvvQes7aKen1jaeLTOGToAL69xuyx7F0oS7j6f+0epzsNJX2qwQBNOk6UtUQBFi4UGf40PdnCo9LsBlRYAAAAASUVORK5CYII="
    },
    "GRAVITY": {
        "name": "Gravity",
        "description": "Increases the effect gravity has on a projectile",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAOElEQVQ4jWNgGGjACGP8////P4oEIyMjpnISwH8ooMgQmEHD3BAmik2l1BX0dwHFAD2BDUEvoAMARvIn5EITMvwAAAAASUVORK5CYII="
    },
    "GRAVITY_ANTI": {
        "name": "Anti-gravity",
        "description": "Applies a lifting force to a projectile",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAN0lEQVQ4jWNgGHLgPxTA+EwD6RjyALLzGRjo7QV02+nrAmy2D3LN6CkOF2DEZRMjIyMjpvLBCAADOyfkIj+S2AAAAABJRU5ErkJggg=="
    },
    "SINEWAVE": {
        "name": "Slithering path",
        "description": "Makes a projectile move rapidly in a slithering manner, like a snake",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 2,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAJ0lEQVQ4jWNgGAWDCPyHAmLFMRSRyobxmSh0OG4X4fIOTSwcBRQCABoYM8/SPrgzAAAAAElFTkSuQmCC"
    },
    "CHAOTIC_ARC": {
        "name": "Chaotic path",
        "description": "Causes a projectile to chaotically fly wherever it wishes",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 2,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAATklEQVQ4jcWQSQ4AIAgDW/7/Z71oQgwBRKM9T4cF+JE2AgDyfHIJ0CuvEQ15cpIMJ3uScP0MmJJEUPmRVyVHp6SfacFbZatUEsxiuRylAx4EZ6E8RAhBAAAAAElFTkSuQmCC"
    },
    "PINGPONG_PATH": {
        "name": "Ping-pong path",
        "description": "Makes a projectile fly back and forth",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAATUlEQVQ4jWNgoBAwwhj/////T7QmRkZGwqrwAGTLmCgyiQHJC+gm49WEzQukhAEyGEReIAWMxgKaa0hxBSMjIyNMDcwbLOgKKHURyQAAdbQoBgrBgMEAAAAASUVORK5CYII="
    },
    "AVOIDING_ARC": {
        "name": "Avoiding arc",
        "description": "Makes a projectile shy away from obstacles",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAcElEQVQ4jWNgIBFM9zb+7y8u/N9fXPg/AwMDAxOpBiADf3Hh/xQZQLQL/kMB2QbgAyzEKGJkZGTEJUe0FygyAB8gaMD/////U+wFsl1AyHYGBhyxAAs0QppRDEAOaWI0YhhAiiZkQJ+8gAtsfPmWEQBTCSUnYveOHQAAAABJRU5ErkJggg=="
    },
    "FLOATING_ARC": {
        "name": "Floating arc",
        "description": "Makes a projectile float above the ground",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAe0lEQVQ4jWNgGAWMuCT+////H66IkRGnOoKasfFJ0kxInIkkp5Fq+3RvY6xe+Q8FKBLYNPuLC/9HNwRdPQsDA+mhjKweq8bp3sb/d5x5AOd7mCgwZG49i1UtIzYnImtGNgSrAf7iwsTHMRZAcTQyMjAwMJDrio0v3zICACAXTSZmz/mMAAAAAElFTkSuQmCC"
    },
    "FLY_DOWNWARDS": {
        "name": "Fly downwards",
        "description": "Causes a projectile to aim straight downwards a short time after casting",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -3,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAXklEQVQ4jWNgoDWQSV78Hx+fKGA97fz/+qMf/1tPO0+aZpnkxf//////v/7oRzhNtCv+//8PV0S2C5BtgxlI0HZkm6kKiDKYZrbT3HCibKC9CwiB/1CALs40EI5BAQDoQlPlIufWqAAAAABJRU5ErkJggg=="
    },
    "FLY_UPWARDS": {
        "name": "Fly upwards",
        "description": "Causes a projectile to aim straight upwards a short time after casting",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -3,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAWklEQVQ4jWNgoBf4DwXo4kx0cwFBgM15pMjTHhDlApo6k2jDqe4KmeTF/5ENhvFJcon1tPP/649+/G897TxprpNJXvz//////+uPfoTTJLmCYhdgs41k2wkBAJTCU+VIMeigAAAAAElFTkSuQmCC"
    },
    "HORIZONTAL_ARC": {
        "name": "Horizontal path",
        "description": "Forces a projectile on a horizontal path, but increases its damage",
        "meta": {
            "action_type": 2,
            "damage_projectile_add": 0.3,
            "fire_rate_wait": -6,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAl0lEQVQ4jWNgGAUYgC9+aQk5cgwMDAwMCtWn/hOyAF0NC4yRsfX7/5VbDzQwMDAwcHq3WrGKKFgxcwtxMzAwMPz9+u7r7zcPjn3fWn3s4/u3DRlbv/+f4c3JyICsWaTs1OqMrd//r7z+///K6/8xAEwcWS0DAwMDI7oL3k/zbMTnAsGs7fXh3g4NKC6gJAwwAEWxMArIAwD02G7eyTnlOwAAAABJRU5ErkJggg=="
    },
    "LINE_ARC": {
        "name": "Linear arc",
        "description": "Makes a projectile fly only in cardinal or diagonal lines",
        "meta": {
            "action_type": 2,
            "damage_projectile_add": 0.2,
            "fire_rate_wait": -4,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAdElEQVQ4jWNgGGjAiE+y9fT//wwMDAxVJgwMjIyMeNViAONp9//D2P////+PTy1ezSQbIJazFkMxPgNYGBgYGLj8Gv8zMDAw8MjpMLyaEkyaX4kB6C74DwUMDAwMTFS3jRgXIAOSXEBydCJrJFszRTYPDQAAwlM7Wuhe/uEAAAAASUVORK5CYII="
    },
    "ORBIT_SHOT": {
        "name": "Orbiting Arc",
        "description": "A projectile orbits the point of its origin",
        "meta": {
            "action_type": 2,
            "damage_projectile_add": 0.1,
            "fire_rate_wait": -6,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAbUlEQVQ4jWNgGLTg6LP//zM2v/5vMeHyf5jYfyjAq/H/////jz7DVPQfDcDEWdAVMTIyMpLiUiZiFSIbjNUSgn6j1AXIADlgyTLAQFkCU5AUL2CLJaINwamZWAOISkjYFOESx5lo0BWTmsDoBwB4Nl8cgEEbcgAAAABJRU5ErkJggg=="
    },
    "SPIRALING_SHOT": {
        "name": "Spiral Arc",
        "description": "A projectile flies in a spiralling pattern",
        "meta": {
            "action_type": 2,
            "damage_projectile_add": 0.1,
            "fire_rate_wait": -6,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAcElEQVQ4jWNgGGjAiE/y/////1EUMzLiVY+iEV0zPnG8tpKkhhjN6GqZiFF89Nn//xmbX5Pn9KPPEOLIhmCox2YAsmaLCZf/41WPK9RJcS1WgK4YI22gS2BLLMia0OUZsZpKbIpjIDIaiQJEJVNaAABAYm9biJhm7wAAAABJRU5ErkJggg=="
    },
    "PHASING_ARC": {
        "name": "Phasing Arc",
        "description": "Makes a projectile fly much slower, but teleport short distances over its flight",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -12,
            "speed_multiplier": 0.33,
            "action_mana_drain": 2
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAdUlEQVQ4jb1Q2xGAIAwjTKIjMYCjOA0DOIojxQ+BQ68g4cP89NprHhfn/gYT8g6VXO8A4AXyYt2HBRLW2l3k3ilaSVqESDL2fpox3kQAm/VndqDE7JW4f7l3MVNYIBlUI2Ty4wgcowJeimkJpHnOuBfIhVW4AKZnQGwCJgrQAAAAAElFTkSuQmCC"
    },
    "BOUNCE": {
        "name": "Bounce",
        "description": "Makes a projectile bounce on impact",
        "meta": {
            "action_type": 2,
            "bounces": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAS0lEQVQ4jWNgGAUkg/9QAOMz4ZKgyAZSxIkyhCyXwTQR0syET5JiQIzTKXYBY3l5OUVRx3LmzBmKXEC5FxgYGBicnZ3J8sbevXsZAaDtNKt9TwD1AAAAAElFTkSuQmCC"
    },
    "REMOVE_BOUNCE": {
        "name": "Remove Bounce",
        "description": "A normally bouncy projectile stops doing so",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAUElEQVQ4jWNgoAX4Hxn5Bh+fJEPI0kyRzVRxAb4w+A8FmJpwSZCqDpckMYbjVEySZnRNhDQzkWwyOa6gqQsYy8vLSQ8gJMBy5swZilxAsRcARqtQJf3MpcsAAAAASUVORK5CYII="
    },
    "HOMING": {
        "name": "Homing",
        "description": "Makes a projectile accelerate towards your foes",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAADASURBVHjaYvz//z8DJQAggJjwSd4oLCRoOkAAMWJzwRlXV7Agj44Ow5crV8BiJrt3M2IzACCAcLoApgGXRhgACCAMFyA7W6O/nxGdj24AQAAxgAxAx6ddXP6D6OsFBf+R+dgwQADh9AIsHGA0zDp0dQABxIDLZGQXYOJ/cHGAAGKkNB0ABBAFBoA0MjICBBDUAAiHHGMAAgjNBcQahFAHEEAUhwFAAGExAJ8rMOUAAgiPC5AV4zYUIIAo9gJAgAEA1ferYmobEfcAAAAASUVORK5CYII="
    },
    "HOMING_SHORT": {
        "name": "Short-range Homing",
        "description": "A projectile flies towards targets when near them",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAdUlEQVQ4je2Quw2AMBBDbeoMQnEtG2SgbJKF2CBtJBahxhSAQMqngZLXuLDvZBn4eQ0BQNoEkN1gwx4u+1CpdkgSSwiF93hwx2uh5L2e2nlQtlhCkDPDGCOdWdHk3KDWTgJIkkjey5lhzRnTPHe3atLa4OcDdtJwJZosDN5CAAAAAElFTkSuQmCC"
    },
    "HOMING_ROTATE": {
        "name": "Rotate towards foes",
        "description": "Makes a projectile turn towards your foes",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAADCSURBVHjaYvz//z8DJQAggJjwSd4oLCRoOkAAMWJzwRlXV7Agj44Ow5crV8BiJrt3M2IzACCAcLoApgGXRhgACCAMFyA7W6O/nxGdj24AQAAxgAxAx6ddXP6D6OsFBf+R+dgwQADh9AIsHGA0LgAQQAy4TEZ2ARKGAbgYQAAxkpgOQIpRwgEggBgpTUgAAQQzAGYyhg2EAEAAIbuAZM0gABBA+LyAy0AUcYAAIhQG6IZgeBUggMhxAQoACCCKYwEgwACEUKRku0jbhQAAAABJRU5ErkJggg=="
    },
    "HOMING_SHOOTER": {
        "name": "Boomerang",
        "description": "Gives a projectile a path that curves towards you",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAk0lEQVQ4jWNgoBVoDFn6nxh1TLg0H729mShDsBrw8ut9hmuPzjK8/HqfPBeIcysy+FnEM4hzK5JuQJZny3+YIch8ogBMcW+tzX8YRhb///8fhmGM2AyCBd7Lr/cZpm2vwVDz//+//4yMTIwMDAwMLNgMIBR4MM0MDHjCABcf2RX4BcgFxBhENctItoFkm2EaaOpkAMiQWSNDcNTcAAAAAElFTkSuQmCC"
    },
    "AUTOAIM": {
        "name": "Auto-Aim",
        "description": "Makes a projectile turns towards the nearest visible enemy",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 25
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAqklEQVQ4y7VSuw3DIBB9z3KVTSK5TeHeI6RmBnsTz0CdEdy7yADONi+NDxHkBBKUk07cCd7nAEpCTTS5AyRDPqZJcQ8AbYnKfRiU1pdlYZEDCwPYapF1sI2jAGDrOp3nmXEPgJD0MWPbBrZeUvkIBorvA8aSSaXjxFj+8x+oloC/Enw1U1uqfOt77Xu6riuPCOwAvPcCQOdcqE87OBVo3igf1cHBi93aZ3wC1Rt7EljiR5EAAAAASUVORK5CYII="
    },
    "HOMING_ACCELERATING": {
        "name": "Accelerative Homing",
        "description": "A projectile homes towards enemies at an increasing pace",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 60
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAqElEQVQ4jbVRwQ3CMAy865tBQOqzG9Duk07SDsEWeTAAPCuVERBv3j1ejqKKpokEJ1m2I9l3vgD/hqQQs3OKewBgzoJ71wkADnWN9zQBABrvSRJVrpLGe8bZsKtgdk5WH4eBj74P/Wkcd+chCbe2VeyB9QDyTzAfLJMJ8vP1pctTIb79Qob0RdISFmxh8wSy2jcoJT1+K1pisovZ10qKB1KsWa4Xs/4KH60pkGp6koEmAAAAAElFTkSuQmCC"
    },
    "HOMING_CURSOR": {
        "name": "Aiming Arc",
        "description": "A projectile rotates towards the direction you're aiming",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAxklEQVQ4ja2SwQ2DMBAE9/KmkCD5STqAfuxKoB/TQfJEgkLyzuaB7BxgO1HEfs5nyau9OQNniCRIbs4kMVtL3ack4REAiAhI4tF1BIDKGDynCQDQeC8icjC45FI13ouuOR0SzNbGrNe+l8W52NfDkDbbM7i3LTWD0KeUHSFwCLWo07ZQUmoDQGGEnDRkZfA9QoAJIFZlEPLljRbnWBmDehikMiYm+ZkBsG4k/M7bOH6gaNLki9t+e79nUEhArqOFqh5lNvKX3uOfvfsDtvPzAAAAAElFTkSuQmCC"
    },
    "HOMING_AREA": {
        "name": "Projectile Area Teleport",
        "description": "If a valid target appears somewhere in the proximity of a projectile, the projectile will teleport right on top of the target",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 8,
            "speed_multiplier": 0.75,
            "spread_degrees": 6,
            "action_mana_drain": 60
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAxUlEQVQ4ja2TQQ6CMBBFX1l7EBcGNx4BLoI72cBJWOEOL8IRdKMhwYO4/26YBGtRSPhJ02k7/89PO4WZkIQk+qKQxZJwU4TmrtriLCa/pakANrsdr64D4NC23/zmrnpMturXJNHYga2dTz7uXe6L9kUhi7dV5Z5lKT8Hv+pcB9EUKQS7B5tnVTcH/isEBfwTDQiJGicCyGJOAM45NyH0McaIAC4PzuNNExrmyV4JWlqau+gVVnHxNyfUyr/2Z30mgFCLr4I3zH6uY8VGH5YAAAAASUVORK5CYII="
    },
    "PIERCING_SHOT": {
        "name": "Piercing shot",
        "description": "Makes a projectile fly through enemies, but harmful to the caster",
        "meta": {
            "action_type": 2,
            "damage_projectile_add": -0.6,
            "speed_multiplier": 1,
            "action_mana_drain": 140
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAfUlEQVQ4jWNgoAb4/////xFsCCMuifsvDvxnYGBgePH+EoOlZh5OdVglYJphAJ8hTPicl5ASy/Di/SWGitJufMqwuwCGc5uM/x+/PglnuGB1gaKEA+OL95cYGBgYGIwsFUizHRnAbMbnApIAVdMJRYb9hwIGBgLRiM8FVAMAl2hVEb8iZ4QAAAAASUVORK5CYII="
    },
    "CLIPPING_SHOT": {
        "name": "Drilling shot",
        "description": "Gives a projectile the power to go through the ground",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 50,
            "reload_time": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 160
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAWklEQVQ4jWNkwAOmexv/33HmAU75jS/fMjLhM4AQ8BcX/k+RAQwMDAyjBpBpwIYXbxg2vHiDMOD/////sdEZW87ANSDTyIARn02EEhLcBZSAUQMoNGDjy7eMADXKJ4MVaIZBAAAAAElFTkSuQmCC"
    },
    "DAMAGE": {
        "name": "Damage Plus",
        "description": "Increases the damage done by a projectile",
        "meta": {
            "action_type": 2,
            "damage_projectile_add": 0.4,
            "fire_rate_wait": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAd0lEQVR4nGNgoDq4Gxl5NzISzmUiVT8jfrPRRJSXLyfZBsIAux8m2JhPsDHHykADLMicAAU5NAYDA4Py8uVYNBx48hyNgQuwQGxHFsKvh+RQQvgBbrCDjCQeS1A87SAjSVAPNKaxhiADA0PBkZPYNWDVg6maHAAA0fUpnwoF+2MAAAAASUVORK5CYII="
    },
    "DAMAGE_RANDOM": {
        "name": "Random damage",
        "description": "Randomly increases or lowers the damage done by projectiles",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 15
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABAklEQVQ4jWNgoBAwEquwftmJDQwMDAwMSm8ZGi28A0iypav+xIb/SKD+RNsGojUfgGreeaJ+w3QoRjaEiZABHAzX4OxMi0a407UZfhLvhZ0n6uFOrj8xZ8P/////n9hXj+qNCTbm/yfYmP/Hx67fdwJDM4YXAhTk/mNj1+87saHB0dz/5PyGFxZOCK+wwBgHnjxnwMaGg3MM5gyODAwMDxlOIguzwJyJrh7dkMYSC0n1zyc2RDU2oqQBgrEAA8vqT2yIbDD3P1E/DyXwWJA5yLY6yEii8KMaLQIYGE5siGpMQnEBigEwjfgNQQXwvIAtHJBBwZGTWPMNiiAuQ3BppgoAAKs9ioJHW/QhAAAAAElFTkSuQmCC"
    },
    "BLOODLUST": {
        "name": "Bloodlust",
        "description": "A projectile gains a hefty damage boost, but is also able to hurt you",
        "meta": {
            "action_type": 2,
            "damage_projectile_add": 1.3,
            "fire_rate_wait": 8,
            "speed_multiplier": 1,
            "spread_degrees": 6,
            "action_mana_drain": 2
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABTklEQVQ4ja2TsWrCUBSGvysSaHHo4iIimUKbLnfQrWMfQMgj+AwZO3XsA/gUTl07WpcouBhDB+lQCqWLQ0wgSzroSW9i0qk/hPy557//PefkXPhP5O5dLg9AoHUoXNYDrcPGzea3KazGpq77KLwtZGnFW4DX+6N49DJXwtWJR0nKZLFSQ8vygAeAVmGQZTPhUZIyHvTzKEkBGA/6OcD15QUAw6xzU8og0DocrdcuwOzzu0h1s4/Z7OOGjhkG4igpH9GlaliHlll/FVGSsosPBReY+jaU6zdP3cUHfMfm6e29ZGzqWwCnrpZO8XpdfMcGwHdsnj++CmPR/xqcejBZrFSUpHi97lk5vmNze9XB1Jd6IMMiJnX9mCxW6iwAx8kyR/gvmOMNxiSqcK6qwaUVb5dZNhtalleknTX/tWLGA61D8y4Il7d5FxpRJ6pb+wGL77XnoP1f/gAAAABJRU5ErkJggg=="
    },
    "DAMAGE_FOREVER": {
        "name": "Mana To Damage",
        "description": "If the wand has more than 50 mana, all mana over that is converted into additional damage",
        "meta": {
            "action_type": 2,
            "action_max_uses": 20,
            "fire_rate_wait": 15,
            "reload_time": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA+0lEQVQ4ja2RMWrDMBiFPxffIWBP2UrvEAcUAlk8ZvfYE4T2BsWLr5C1dNQSCBXEHXMAj5kk6CnUxSpqJTUO9E2PJ733P/3KiEDX9cLxUsqPa3pgtsNg7TDYc9dZZ9J1vTh3nXVnTs9/m4u27c1udwD2BcyNEL2GygjRF0o9G6UuQFO0ba+hysMOAOxLKV8BNCxH86GU8mUcBLAJGnhoxktzI8SmUKoamzwBF6BJ+G7fQeaMb+ulBdgeT1mKx34heMLjw72Ncd/k4zvgXX8S49eQ+/V9TA25mzzqrwaxqatyNqlFsMRVObspJPjGFLbHUxbTf4ipkJT5X/AFQR+bYYKfD2wAAAAASUVORK5CYII="
    },
    "CRITICAL_HIT": {
        "name": "Critical Plus",
        "description": "Gives a projectile +15% chance of a critical hit",
        "meta": {
            "action_type": 2,
            "damage_critical_chance": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA2ElEQVQ4jWNgGLTg4sWL/y9evPifkDpGfJKLFy/+z8DAwPD06VOG44cPMijLSjP0zZiDoocJnwGxsbGMMM0MDAwMYgoqKC4k6AIGBgYGf2/P/wwMDAyWtvYMQd++MTAwMDCcVFVlML99m0GtqYkRrwuKMlLgmqWlpTE04/VCUUbK/7uPnzJs3LqdsaKighFZ8+mTJ+HqsBpwYkM7XDOyuJ6eHopmnDbD/A0DFy9e/H+rru7/Unf3/wwMDAwwGqvN6JqRAU6NuGwm2oCijJT/sBCnKcDrhQEDAKAmZoLIx6XNAAAAAElFTkSuQmCC"
    },
    "AREA_DAMAGE": {
        "name": "Damage field",
        "description": "Gives a projectile an energy field that constantly deals 3.5 damage to nearby creatures",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAhUlEQVQ4je1RsQ2AMAyzESfwSRdeQN0RM4cxV+yIF1j4hB/CgIxKVUEPwEvjqLacBPjBkk/HZBbzZuStq0vE27oPaU8mrwlyYqHtXGhGsgIAs2fElAOAn13wswtpvwIA8oojIUmqzoliPHYgI9XHZLb01wh6U3xeIbcHzV9kIJOYx2f8AZxg/j+VHiN0vAAAAABJRU5ErkJggg=="
    },
    "SPELLS_TO_POWER": {
        "name": "Spells to Power",
        "description": "Converts any nearby projectiles cast by you into extra damage",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 110
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAxElEQVQ4jWNgIAH8t3H5T5HClSLSJ4g2DFkxDEyVVSrAJ48BkDUg89HFSTKEZAPQNZKleaqsUsF/G5f//1Mm//+fMvk/VkOuCkr+hym+Kij5/7+Ny///Ni7/UTTDAJIhTAwMkBDVfv+ccaqsUoHDl+/92u+fM167epWBgYGBIUteqR+f6xgJOd3hy/d+LW1tiICGP4S+sZGB8cgeRoIGoBsGY2c/vjeBWH2URSOyQlxsnAA9qaJrwpuU0TMKNsVE50xCigF6e2vhMLgjVQAAAABJRU5ErkJggg=="
    },
    "ESSENCE_TO_POWER": {
        "name": "Essence to Power",
        "description": "Increases a projectile's damage based on the number of creatures nearby",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 110
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAB50lEQVQ4jY1TTUgbQRT+VtdawYOHQoSYbhFBMKVdGqK35JAopIgJhZ4qVCiVFjrpQajEg2gugdySOXiQCIq5iZhSV5AuaAUF2xQpWAjUlkKVlMJS8ZRSfJ7esq6/HwxvmO+9b94bvlHggrG6Trx/2BdW3Lwbqrs4lc4hGgri/YePMFbXyS3ivqDeSbb6OieeP32M6blFZMZfw1zbxNaGOcm8pkvR1LAXm55bhELHODhKWHVOgQWjOWmubdrFRaO75G65aHSXoqHguRwAYGQsQ5ouBcfzclp1c+mSZzkNp4imS8Hr2gJcOCBrVLCIChaR/8UGnUlq76+UAcAfXhaBwf1yz+guDcgaaboUPaO7VLDIRsEie7w6nun7u86AP7wsbvr0ofK8N/B/v+Xz71/fcP/ZcN7T1nFhd5caRdOluHX30ZDq/fvA09aB+JMbAIBS8R/eikYFcBlJ06WooSvSiK/mzx0hAaA87w0wV/l0Ow8Aldm4fbHC5FT2Xp4PX775kqyhK1LdiSScwjNZNc457FDbSKl0zo7chbO7mawad+awpW2BaCh4KvIIDHZoKp2znQoA9QBwWF3ZPjhKWE0Ne7EFozmp4s+Pw+rKNhcz39/riSl0jPY7PrwaHrzyp57ByFiGnBEATgARkd79pbTf4gAAAABJRU5ErkJggg=="
    },
    "HEAVY_SHOT": {
        "name": "Heavy Shot",
        "description": "Greatly increases the damage done by a projectile, at the cost of its speed",
        "meta": {
            "action_type": 2,
            "damage_projectile_add": 1.75,
            "fire_rate_wait": 10,
            "speed_multiplier": 0.3,
            "action_mana_drain": 7
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAd0lEQVR4nGNgoAW4WVZGmoYPJ0/C9TDiMVU8OBhZ5OXatepdXSTbQBTAonqOi+YcF02sDDTAhMzxUhBGY2ACFgi17s4rNAY+DWhW49fDhEcOn5OQDQ5SEcNjCQsyJ0hFjKAeaExjDUEGBoaUPdexa8CqB1M1OQAA0B0wxWWNPoQAAAAASUVORK5CYII="
    },
    "LIGHT_SHOT": {
        "name": "Light shot",
        "description": "Makes a projectile move considerably faster, but deal less damage",
        "meta": {
            "action_type": 2,
            "damage_projectile_add": -1,
            "fire_rate_wait": -3,
            "speed_multiplier": 7.5,
            "spread_degrees": -6,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAhUlEQVQ4jWNgoBfInn3zPzJNMph9/P9/sgzJnn3z/+zj/+GYbJdQpBlZE07NmjFz/mvGzPmPj40NMKEL8OsE/MfGxgVYYIxnF7YwYGMTZQA2JxJrCIYXSAUsyBxkW6UMfIhyBQu6gJSBD0mGMMIY+KKKgYGB4fqSFEZs4iiCuAzBpZkqAAAyYFqzrPhNZwAAAABJRU5ErkJggg=="
    },
    "KNOCKBACK": {
        "name": "Knockback",
        "description": "Gives a projectile the power to knock back the foes it hits",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAArElEQVR4nK2QsQ3DMAwEn4YAI4Xq9O4yh72NoGmCbOERvIdmUJ1CjQGlYMAwNAOk8FeU9CTvBZyu3rs+Dv+YtMh1E5Fp4xtng3aLSTc7G1wwIvoa0UpqJblukyewe5xmAK3gcnv8AuPaZnjerzriMRsJkrzt2xpzbSXxNrMniI+pAGBBKxinmQnNdwXB0Nq3Fcs7j8EbAMRcTUPMVXiMgh75qQGdwUoSH9lO0AuxzmFW+esolwAAAABJRU5ErkJggg=="
    },
    "RECOIL": {
        "name": "Recoil",
        "description": "Increases the recoil when casting spells",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAYElEQVR4nKWPsQ3AMAgEIUtkSMbJHl6HQVIgN/4UFEayooDz9d0/EP0JgE/mqJbyWs/ML3BYyBwzBQCxNS8TAKezTqRNpbCTRdfc17mjmUrBLDzj8W5TGb0VtNFbTdjPAwTmNpf2GzvnAAAAAElFTkSuQmCC"
    },
    "RECOIL_DAMPER": {
        "name": "Recoil Damper",
        "description": "Reduces the recoil when casting spells",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAlklEQVQ4jWNgGFTg82Rxexj7+/n4+WQbQIpmJmTNvLkvDyJL/v///z9JNqPbTrQBuDT//////5HNc4kzBJtmGFjWXUzYEJgBMGffP7+TdEPQwf/////fP7/zP7LB6DHEhE0jMlA0dGe8f37nf0ZGRkaSXQADMFdgSx8EXQBzBTIfOdpZiDEAZjOn4cJEYtTjBci2UwUAAPU5dmgGMte4AAAAAElFTkSuQmCC"
    },
    "SPEED": {
        "name": "Speed Up",
        "description": "Increases the speed at which a projectile flies through the air",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 2.5,
            "action_mana_drain": 3
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAP0lEQVR4nGNgGKygZ/9PIgWh4P///5jSWAURcpjSWAVR5NCksQqiyyFLYxVkwukt/IBMJ5HmadKCleSIG0wAAADViGvFLudeAAAAAElFTkSuQmCC"
    },
    "ACCELERATING_SHOT": {
        "name": "Accelerating shot",
        "description": "Causes a projectile to accelerate as it flies",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 8,
            "speed_multiplier": 0.32,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAX0lEQVQ4jeXRsQnAIBBGYcnKVtnBMSwcIotZvTQRjt+IyjWBvFLODzxD+E+lJnaGLz0DGCKlplOGsyI8vSJAtAiQFcHUIUC0SAMsgtSQY7qQldxPcC9R2/7GlVyXv90NMHasVcDJ/HgAAAAASUVORK5CYII="
    },
    "DECELERATING_SHOT": {
        "name": "Decelerating shot",
        "description": "Makes a projectile decelerate as it flies",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -8,
            "speed_multiplier": 1.68,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAYklEQVQ4jWNgGLngVaDVd2IVnsCm+f////+xKZ6Mrvn///9LsGnGMOBVoNXk////F6NrRjYAWTOKATDNMAOQNcMMQNcMM4CJqIAgBlDkBWRD0PjEByIelxEfjaQAohPS4AcApRKdy3arY3MAAAAASUVORK5CYII="
    },
    "EXPLOSIVE_PROJECTILE": {
        "name": "Explosive projectile",
        "description": "Makes a projectile more destructive to the environment",
        "meta": {
            "action_type": 2,
            "explosion_radius": 15,
            "fire_rate_wait": 40,
            "speed_multiplier": 0.75,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA5klEQVQ4jWNkwAOmexv/33HmAU75jS/fMjLhM4AQ8BcX/k+RAQwMDAwoBnxu5/5PkQG8lV8ZYezvh/3/MzAwMHiYKDAwMDAwLCn4gdUARqyiSIZw2m5knO5t/J+BgYEBW4Cy4NOI7BIGBgYGhg5MQ/C74Hk7QvOdE3DmnyN7GGImcCBc8P////+MjIyMyPSPIwFYDV3Q8YRhxxkOOJ+JgYGBgZGRkREbjWz7go4nDAs6njAkVMgQ5wVkv8M0RoQcwVCHNSF9bueGByLEyQ8YIkKOYI1KvIFIKC/gdAEpYGAN2PjyLSMAlfhfaNk8Ov0AAAAASUVORK5CYII="
    },
    "WATER_TO_POISON": {
        "name": "Water to poison",
        "description": "Makes any water within a projectile's range turns into poison",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAuklEQVQ4jWNgQAIySYv+M5AImJA5ogqapOpHNUBLXpR8AwybTv9HpslyATmuYILZCtNopydPkitQXGCnJw93xf///7Eakm6yFrvhhk2n/0cvfPA/euEDuAKYIR8etsHF2t0fkBbV////////2wG4pg2RqAZgBCIy+PCw7T/D94MMDJz2KLYju4IFnwEC8lWM2MQ1hYh0ATJod3/wH6ZRXlMK7gqiDYABeU0pFFdgdSIxrghYrkCSXtoBABohSE0bSE7iAAAAAElFTkSuQmCC"
    },
    "BLOOD_TO_ACID": {
        "name": "Blood to acid",
        "description": "Makes any blood within a projectile's range turns into acid",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAxElEQVQ4jWNgQALlQkL/GUgETMgcUw4OUvWjGqClqkq+AWukpP4j02S5gBxXMMFshWlUCgsjyRUoLlAKC4O74v///1gNse3Twm74Gimp/9fs7f9fs7eHK4AZ8uFhG1wsdZ8+aVH9//////+/HYBrmvQgEsUAjEBEBh8etv1n+H6QgYHTHsV2ZFew4DNAQL6KEZu4rpIWAwPDRcIuQAap+/T/QzQyMNjLJMFdQbQBMGAvk4TkCgYGrE4kxhV5CstJ0ks7AADHG0RrzeYKJQAAAABJRU5ErkJggg=="
    },
    "LAVA_TO_BLOOD": {
        "name": "Lava to blood",
        "description": "Makes any lava within a projectile's range turn into blood",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA0klEQVQ4jbWRsQrCMBRFr8HFRaQ4SBYhEJAM/kI3Qdo5/9FJ8AP6Nf0C6WBnNwf3UujcSaf6nJqmGLQRvFseueedEMDKZT8neIbZh9Vm6dsfAnikfgdUiSC23aFKhNczDKDb7mvB7O0A4GthDJ7XkxnySIGInJBDELjhVSKozWNq89hc6CBNmZpZxrnfVxMR0f1sSrcwHADYe6VPU6aERwHMwsF222L6CbBYHyeuuZISqOvvBnYyzklJCQAQWhuL0YAuQuveAoBTcYyFKgqv7v/yAmvTShC7jmONAAAAAElFTkSuQmCC"
    },
    "LIQUID_TO_EXPLOSION": {
        "name": "Liquid Detonation",
        "description": "Converts nearby nonmagical liquids into explosive gunpowder",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA00lEQVQ4jWNgQALKxln/GUgETMgccRldUvWjGiCraEC+AVb+0/8j02S5gBxXMMFshWnU09fDcMXndm6crkJxgZ6+HtwV////h2virfzKSNApVv7T/4cXHP8fXnAcrhFmyIeHbUS7CAP8//////9vB+Aa0DXjddqHh23/+UWtGBg47RkYGRkZvx/2h2vmtN3ISNAAZPD9eTvC5jsn4EwWXBo+t3P/Z7FxwWsop+1GRqJc8P2w/38GFQu47TDnI3sJxWZsBsAwTo24AMxAdE0kRSU+AAAE32GAgpNalwAAAABJRU5ErkJggg=="
    },
    "TOXIC_TO_ACID": {
        "name": "Toxic sludge to acid",
        "description": "Makes any toxic sludge within a projectile's range turn into acid",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAxklEQVQ4jWNgQAKpfVr/GUgETMgcLQNWUvWjGmCgpEW+Af379P8j02S5gBxXMMFshWnUkkkiyRWogSiTRJYrGGCu2P8g8v/+B5H/GRgYGP7//4/VFUblSsSHEcyQDw/b4JrCl2qgGIARiMiAkZGR8f/////5Ra3gYpqy6ihq8Brw4WHbf4bvBxkYOO1RbEd2BQs+AwTkqxixiUNccYOwC5BB+FKN/zDni4qqwF1BtAEwICqqguQKBgasTiTGFQ12G0nSixMAAJL7QXeKNCNZAAAAAElFTkSuQmCC"
    },
    "STATIC_TO_SAND": {
        "name": "Ground to sand",
        "description": "Makes any hard, solid materials within a projectile's range turn into sand",
        "meta": {
            "action_type": 2,
            "action_max_uses": 8,
            "fire_rate_wait": 60,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAtUlEQVQ4jWNgoAb4//9/339U0EeqAf/x8fEBJpJsooUBLFC6H83Z/ZQaTDkgJSAJGvLhYRv5hv3//////28HyDPgw8O2//+/HfiP7J3ZXenku2Z2V/r/mgz//8gGseDXggBbl7X/V1ATZGBgOM/AwMDAkFo2k5GBgciEBNN8bN95BikZBQaYKxgYGBiYCWlO8rX/LyDIwvD4/guGh/deMNi66jLw8ogwKInyNBw6c7ORWB/gBAAi5GF+XMeU+QAAAABJRU5ErkJggg=="
    },
    "TRANSMUTATION": {
        "name": "Chaotic transmutation",
        "description": "Transmutes various liquids and powdery substances within a projectile's range into something else",
        "meta": {
            "action_type": 2,
            "action_max_uses": 8,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAATElEQVQ4jWNgoBa4vOU/HJClGR+faPD/////ZGumyGZcrqFYE4z/4WEb+S77///////fDpBnwIeHbf//fztAXtRiA6NphUqAajECAwDCHWEXmmAAqgAAAABJRU5ErkJggg=="
    },
    "RANDOM_EXPLOSION": {
        "name": "Chaos magic",
        "description": "Makes a projectile launch a random spell (out of a limited selection) when it hits something",
        "meta": {
            "action_type": 2,
            "action_max_uses": 30,
            "fire_rate_wait": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA+0lEQVQ4jaWSMW7CQBBFv6joOQRFDgDH4AiGLqcY0VDkAmmA0vU2FG6sFBRDg2QhpbQUGgqfwNVPwc5qs9ghhC9ZGq/3PY9mF/CRUh3+GynVkaRJJFcnuTrRXadUZXu7bvCbXGUW0ZVLYZLslHx4uFBx7/7pk1g9iD8M8Rnq1+lyZvUL2h8/mi4XM/SlUAl20fW13VIeH7AN9jl4K5d7ewcAkEtyB46YAAC+cDhvxkyhpsoY1nM/+VQSvzdVxtN8xBu4a3MsteM6b8Zsqoxs6wCbsDcq+wCf5iOyrcm2ZlNlv4PxMVrSDnolhb95qcSAroH+qYO4k7uCR/MNVJTmfHZKklIAAAAASUVORK5CYII="
    },
    "NECROMANCY": {
        "name": "Necromancy",
        "description": "Makes corpses of creatures killed by a projectile rise to your aid",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAmklEQVQ4jc2S0QkAIQiG7WgGFwnaIWjcoB2EFnGJuyfDzO5e74cgyF8/NYBfqrd2M/Nyemu3FxusMeX8mnwQQal1+uaFmd0KnhBx+i77IEdXHERLAt1O1IEWFQAWXI9yIbAB2nzSTFBqDYMIZOongyXdJCsUIr1Gb53RmmWNHoUe7tbCF2bKGU6faWtB4+uWPLkEQoGIQYb7Xz2fBYVM/wVHuQAAAABJRU5ErkJggg=="
    },
    "LIGHT": {
        "name": "Light",
        "description": "Makes a projectile illuminate its surroundings",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAA2UlEQVR4nJVSsQ2DMBC8QCRSgCUGgJImG1gRE7ghazBP1iCNJ4gQDTUNHWIB5AqEkFI8Mo6JkLjKPt+97b8HTuKyp3KR6vVLfo4MuUifdxcAFwxAJRWAollMm2upuWBR4hETJV6UeP4wB2Fctx2RV0sN4PZ46ypjmXHBAAWkdI9DB3/VessFo6euBv1LS22RJFtvoPIH0ALnWLfHaqAOHkALHBjpjGW2l2ryp0tFs1ANy0PbSqqiWYjZkjajsB5jhr0lXbddEMb+MPftRGFXUvXtZI3G6eE7jS9ZTF37xm0pjwAAAABJRU5ErkJggg=="
    },
    "EXPLOSION": {
        "name": "Explosion",
        "description": "A powerful explosion",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAeElEQVR4nGNgGADwuZ2bOupwKvh+2B9TEbIgTj1YjWTCZfz3w/4sNi6YBjNit+F5O5R15wSE/nNkD2/lVwYGBhZk57LYuGDVz2m7kYAHvj9v//68Hdl5KCrQPPf9sD8EYVGKCSCasYYstQEux6DHAxz8ObKHOjYDAHKZPYN9uCtQAAAAAElFTkSuQmCC"
    },
    "EXPLOSION_LIGHT": {
        "name": "Magical Explosion",
        "description": "A large explosion that doesn't damage the ground",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA7ElEQVQ4jc1SMQ6DMAy8UFQpApZulTr2ER1gY+c97JXyjX6BnY0OPIKdB1QoC5I7VI6cNEh06y3EJne+MwB/Dzs0xOeXySh8n8hCXniZjLjmZ9EuCgDsbCgqwBdik7hnZ0P63KqoQIi0rIHrzZ3tbGh93GGHxrk7bE1Ky/rTOF2Q5EcnlORHrM/euVUh0Zs8jY4IAJhGj+xFkE0HJk+jF0t+me0dCBK7WZ89AEBXXXyJRbsol50jCKRlDV11Si7RE7BDQ7rqFFvniWEkXXXqK7JcopxgZ0Oc2Q4NyZ9oF5yQWNwuQqwOXf7kZAtvW2iGEejkLKwAAAAASUVORK5CYII="
    },
    "FIRE_BLAST": {
        "name": "Explosion of brimstone",
        "description": "A fiery explosion",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAApklEQVR4nJ1Suw3DIBB9hjJDeBZXiCncZq00WQFRZRYPEckNcopnvRxnV1x1J97nHgCM1XedB5lOxQqxn+wcUwbQanm8NjcKFhz6WOaYssbrwichptxqATB9No7Hcp/qJBDNEpRNTNmu1GXgsaB0kxxpQQTJOAdr3hEUVMLKc0PQPbhl3D6dAy/KyV9DBwVotci91UJhG+D/GursX2C/v58Yq/Hf+QOkcmWPrQfcCQAAAABJRU5ErkJggg=="
    },
    "POISON_BLAST": {
        "name": "Explosion of poison",
        "description": "An alchemical explosion",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAABE0lEQVR4nGNgIA9Ua9eTqRMZTHefPd19NpybL1/MwMDAiGyJS4AHAwPDjTNXMnemwrkMDAyOrZZwZcxw1TIyMm+evRKREhOREpN5JyMjIyMiJQaRteCy2Hp3M4TNBKFkZGQgjBtnrsC5EDaERLfBgMPo06dPfHx8DAwMb569gktD2Jk7U+EiKH6AsyGu37NhB8Q2DRMduDeY4IparzaiOU9GRgaimoGBYX/1cYggC7INaD558uQJmghKKDEwMPDx8WmY6IhIiUGcbuZk9ebZqwunzv368gPuDagGOzEHlwAPuPSTJ0/4+PjePHulYaKDrBrhh9arjXAvPnnyBOIeuL/RQwUS5xAhuCiEjZw0SAPkp04A7Nx279DwuLQAAAAASUVORK5CYII="
    },
    "ALCOHOL_BLAST": {
        "name": "Explosion of spirits",
        "description": "An inebriating explosion",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABMklEQVQ4ja1TIUzDUBB9ZVQisE2apUtAVLcCMlWDoL7YpgoxVxwCgQOHqMIPS5jAYElYE2qYIesPWzKzkCXYikPdz/9/DYa+5OffXXLv7t2/D3SFPHaoMzIT41GfxqO+VqC1YB47NC+GRFUqE/LYIapSoiqleTHcStpRkwPPxnS2BACEvitj9esnAGBwdACzk102As+WwYf7F4S+K2NMyrcKy9SkErXh7O7LUn0p4fZpZQFAKRp5kixCkkXSBwCqUmolUEm4E9YeeDYCz0bou1skWjs8NBOlaDSC6WwppWivwHbou0iySBLcXJ8AAIpJrSUDQI+N48O9q/PTAVbrHzy+fWMjFihFA2e/h493gSSLsBGLv4dYTGqpmYfG98XlsyZL20TVyWOH2nxzgf6FTj/eLytdmBGs+u0CAAAAAElFTkSuQmCC"
    },
    "THUNDER_BLAST": {
        "name": "Explosion of thunder",
        "description": "An electric explosion",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 110
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAwklEQVR4nJ1SWw7DMAgzO1GkcKPmY71TeqJRiQNNYh9sNI92k8ZXYgw2JMAfcX88T5EZf4eZAcg5D4h9ws9d/dzMeZ6KMwCKNBHlnEspAESk1kpEs51by04piQiAWmspZVmW4IXhQ8F7e6hqXNd1bRUOUTNj5pSSX5nZvQHYtq2z1DJUVVUdcfawg0PBJxYRZhYRdzX46Qq8Jiz56L6Mfd9HS74EbxyWiMjMwlj7GuPrtJpfst2POE1d1uD6R/6CpngBct6ZgLNABqIAAAAASUVORK5CYII="
    },
    "BERSERK_FIELD": {
        "name": "Circle of fervour",
        "description": "A field of berserk magic",
        "meta": {
            "action_type": 1,
            "action_max_uses": 15,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA50lEQVR4nKWTIQ7DMAxFX6cqaNJA0dQLFPVeI0UFPUJBUcjuFZQLVEMBlYaqgZGlS9xkA/0ksmN/fzsOHESRu3g+HvfQPl+vt1RcmUs005j0S6JCBplpXAEVuCO77YeI5CSTL03VfZIApI2Zxqi9jeADtVinfdKlqbrFOi0URSiC6qF/FXEq18puiIFsAEprcNS6Yu4c9U6NbGGTvVinS2t4NS0Vcwf4c/1FoIKelaPWXgFASsH2jKk5+MohHLVu+0H5p5QKIjWi8m8FGRXwnf4KKLlIu7+QW+W2H4A/q5wi8sh9psN4A8t7fPNPR0XfAAAAAElFTkSuQmCC"
    },
    "POLYMORPH_FIELD": {
        "name": "Circle of transmogrification",
        "description": "A field of sheep-like magic",
        "meta": {
            "action_type": 1,
            "action_max_uses": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABFUlEQVR4nGNgoBAw4pL4+OT3dGQ+vwxrJlEGwDTeO/seRVzJWBCrQSgGfHzyezq6RnSgZCyIYgjcADTNv7DoZcNmCAsWhb8M/ETyGBgYGC5sejMJmY1uENwFSLbDNd+5c5dBRUUZw3SoQWwwVzAh28yj9TEPxkHX/GFJJgPDllIGA6bOPGQvMjEQCebMOcLAwMDA0NOzA0Uc2QC2L9f4J925c5fhzp27cEEYOyXFhuHDhy8MJSUemAbwy7BmwuJZ5EQPA7JBIid6GC5sejNpxYozDA/4midd+Fc+SclYkA0WCxheEBDgYWBgYGDYUxDB8OUa/yQYH9mlyBxsCekXVBFWGmdCQjaEgYHMpIzNIBjAlZkoBgB5Vn4K5XOn2AAAAABJRU5ErkJggg=="
    },
    "CHAOS_POLYMORPH_FIELD": {
        "name": "Circle of unstable metamorphosis",
        "description": "A field of transformative magic",
        "meta": {
            "action_type": 1,
            "action_max_uses": 10,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAApUlEQVR4nGNgoBAw4pL4+OT3dGQ+vwxrJlEGwDTeO/seRVzJWBCrQSgGfHzyezq6RnSgZCyIYgjcAGTNSbmxGdg0z5u8eAa6IUzoinBpxiXHgsvpMNuwab539j3Dxye/p/PLsGay4HImMS5iYMDiBXya0Q1nYIB6gRDAphEGsMYCIYA3FpBBUm5sBtFhwC/DmglLbcTazsBA7aSMzSBkFxJ0HjkAAJBMUnGiU8FnAAAAAElFTkSuQmCC"
    },
    "ELECTROCUTION_FIELD": {
        "name": "Circle of thunder",
        "description": "A field of electrifying magic",
        "meta": {
            "action_type": 1,
            "action_max_uses": 15,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 60
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAyklEQVR4nGNgoBAw4pK4//HfdGS+Ij9TJjZ1LLg0TtvwE6s4ukGM6IrQNXqZMWdsO/V3BoyfFcCOYggTPs2pPmwZ0mLMKGLTNvxE8R4TAw7gZcacgUsOGTBhs93LjBlu89NXfzE0IbsCIxCRNTMwMDBIizEzpPogXDN7y68ZyOoxvIDuZ2SAzTUYLkC2Ad0ryLGB4gJFfqbMrAB2DNNxaUaOSpyxgAyw2YziAlyuePrqL0agoSckjLyAKynDDMeblLEZhOxCXGopAgCZsmb029FmzwAAAABJRU5ErkJggg=="
    },
    "FREEZE_FIELD": {
        "name": "Circle of stillness",
        "description": "A field of freezing magic",
        "meta": {
            "action_type": 1,
            "action_max_uses": 15,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA2ElEQVR4nGNgoBAw4pK4dP/1dGS+nqJoJlEGwDTuOHwRRdzDVh+rQSgGXLr/ejq6RnTgYauPYgjcAJhmET2zjJXXvjCEa/GgaISJvbl0agayIUzoNhgosTGEa/EwGCixYWhGFkNxAbrTXQJsMi7c+8VgoMTGgEy/uXRqBrpXMFzAwMDAcOHeLzgbpilRnxdrmLBgE4Q51ZCXnWEPlO5evHcGNrVYXbBnwxGsinEaoKcomgmLZ2SAy1a8sQADhrzsBA1CMQCXK/DZzsBA7aSMzSBkFxJ0HjkAAAVYX0pMI11FAAAAAElFTkSuQmCC"
    },
    "REGENERATION_FIELD": {
        "name": "Circle of vigour",
        "description": "A field of regenerative magic",
        "meta": {
            "action_type": 1,
            "action_max_uses": 2,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA2ElEQVR4nKVTOw6CQBB9EBMuQbExFFbEHipjuQeBM2ApZ5CDbGmsoDdUFsZQcAk7qyGzn1ETXrU7nzfzdmaBlYgkx/g2F37PE12H4jZS4tA3QbtLFLlBlJhu04r75tfcAUBRthZJHEomaGWglbFsQ99Y8mII2Kl98Owi4tWpbUrIcAYAPHECADym+yKHpHiP6LbMiTIFmElbPlHCv/A6MJP+KYFjGaM7heNBV5zgejMd+fgoRQm8WqgyYSHIE10XZevJcR/NXSTvL0irTORfVzlExDuUYlfhA02EZQMiH5PjAAAAAElFTkSuQmCC"
    },
    "TELEPORTATION_FIELD": {
        "name": "Circle of displacement",
        "description": "A field of teleportative magic",
        "meta": {
            "action_type": 1,
            "action_max_uses": 15,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAmElEQVR4nGNgoBAw4pJY9vjrdGR+lCx3JlEGwDQuPPkTRTzenB2rQSgGLHv8dTq6RnQQb86OYgjcAJjmHcGCGQwMDAwea9/PIMYQJrzWEQEYkW1HsyVj4cmfBF2B0wULT/6cEW/OngEzDJc6vF5ANoQsAwhpZmDAEgu4AHLsUD8WiHUFAwOehIRsCAMDmUkZm0EwgCszUQwARhZPHjg5mr4AAAAASUVORK5CYII="
    },
    "LEVITATION_FIELD": {
        "name": "Circle of buoyancy",
        "description": "A field of levitative magic",
        "meta": {
            "action_type": 1,
            "action_max_uses": 15,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA90lEQVR4nGNgoBAw4pL4fj5+OjKf03BhJlEGwDR+XL4ZRZw/0herQSgGfD8fPx1dIzrgj/RFMQRuALrmqRwVGVpaagzXrt1i0NJSY3A8lzQDmyFM2GyBaWZgYGCA0fuN5mVgU8uEzfaHD+8zXLt2C85HZjMwQMIHFlZYXSAvr8gAcz7MFeiGoLgA2enILkDW+PDhfcIGZP/omMHAwMDQJb5yBszpMMNgYugAZyzgAlM5KjKqAq/OwBkLZS/DMxgYGBiWBJ7KEO98myHe+TZjSeApeAzAXInhAmJdgTMhIRvCwACJKpjNMevNZhCVlLEZBAO4MhPFAAAJtoOptmm41wAAAABJRU5ErkJggg=="
    },
    "SHIELD_FIELD": {
        "name": "Circle of shielding",
        "description": "A field of protective magic",
        "meta": {
            "action_type": 1,
            "action_max_uses": 10,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAzElEQVR4nGNgoBAw4pK4/fz3dGS+qiRrJlEGwDTuufCF4cqDPwwMDAwMOgosDC4GPFgNYkTXjKwRHcAMQjaEEZfmggABB2TNEzZ8OIDNECZkRciaLzz4wYCMYQaiu44J2XZkzW8+/GE4cOEHw4ELEDayIXsufIGHFdwF6CYj8/HJoXiBHAA3QEeBBUUCmY9PDiUWJmz4wMDAgAgHZGCgwAGPiYIAAeyxADN5woYPBwwUOBiQMXI0IgPqJSRkQxgYyEzK2AyCAVyZiWIAAMCjj+0xWu3wAAAAAElFTkSuQmCC"
    },
    "PROJECTILE_TRANSMUTATION_FIELD": {
        "name": "Projectile transmutation field",
        "description": "Projectiles caught within the field transform into harmless critters",
        "meta": {
            "action_type": 1,
            "action_max_uses": 6,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABGElEQVQ4jWNgoAb4/////8FlEMWuIMUlLPgkX3T934JNXKKM0QenAYyMjIwwzTdPP30ME3/w+iqKwTBDGHHZjK55k0VHBgMDA4PfiYoZDAwMDO5ebrISZYw+TLicD7MRpnnP9HMMDAwMDM4FKzOcC1Zm4PQCDPwNPJLxYD3DDBjfJdOI4dWDdwx7T1TMUBDVZlA3ZZDFawADAwPDK43+DGFNUwaL6+wMJwR+MogpCDEwvEBVg9MLb6+fZhDWNGXYt2UTAwMDAwPn2XcMFh/YMdThNEDsRuGMt9dPMzAwMDBcvPucQV9ZkkHsRuEMBVFtlBjBGgsMDJCY2Llt12NkMQVRbQYGBgYGdVNpWbzRiGwINnHkhEQxAABcg2/NI4E5/QAAAABJRU5ErkJggg=="
    },
    "PROJECTILE_THUNDER_FIELD": {
        "name": "Projectile thunder field",
        "description": "Projectiles caught within the field transform into blasts of lightning",
        "meta": {
            "action_type": 1,
            "action_max_uses": 6,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 140
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA/0lEQVQ4jWNgoAb4/////8FlEMWuIMUlLPgku07834JNvMyC0QenAYyMjIwwzaf3Pn2MLv/68VWGrhP/t8AMYcJlM0yzUSBfBrLm/dNdM7Yt2PUY5jqsBiBrOLf+0wwY+5+XTEZY27MZorLacDU4DVhVJZWBrNA9zzLjoK8miotwgvZrn/47ZOycDqP////3////f/8dMnZOt9t09f/////+4/UC8yferaKy2gzn1n+asX+6a0bH9S8MMKczbXsyo+P6F8JegIGwtmczYOHAwMDAICqrzbBz0nE4H6sBZRaMPqbO0rLo4i91P2YwMDAweCW4ycKikRGf7cQkJIoBAG9Yhr6JjeWeAAAAAElFTkSuQmCC"
    },
    "PROJECTILE_GRAVITY_FIELD": {
        "name": "Projectile gravity field",
        "description": "Projectiles caught within the field are attracted towards its center",
        "meta": {
            "action_type": 1,
            "action_max_uses": 6,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAsElEQVQ4jWNgoAb4/////8FlEMWuIMUlLPgkX3T938IsyCCLLPb3PcNjiTJGHxifEZ/mm6efPobxH7y+ysDAwMCgIKrNoG4qLQszBKsByJphGmFAQVSbgYGBAW4IEzYDYM6Gaf7558f9n39+3EcWg6nBagAygGlEZ8MAQQMIAYIGsLNwKGJj4zXg73uGxwwMiABjZ+FQhGlWENVmePD6KlwN0dGIDAhGI7IhhBISxQAASaFao3dqPgAAAAAASUVORK5CYII="
    },
    "VACUUM_POWDER": {
        "name": "Powder Vacuum Field",
        "description": "Sucks powder-like materials nearby and releases them upon expiring",
        "meta": {
            "action_type": 1,
            "action_max_uses": 20,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABJ0lEQVQ4jWNgoAXoiZ3//+vt///RaWxqGdEFvt7+/7+6rHYGNsUyPMoZJYsTUfSgcHpi5/9/8uUuVs0w0NrVnMGtygjXx4RsMzbNN59eeYLMry6rndETOx/uHSaYzdicffPplSdBIeItN59eeYJs0JMvd2fAwoSJgYGBIbMuAcOpMM0P771gYGBgYIAZBJOf3rQA4QIYB1lzToF5i5WTIYO8kgQDAwMDw7o1L2vUpXVkYGpglmK4AKZZQU2Q4di+8wxSMgoMxkb/W9BdiNUFN59eeSIp/r7l2ZMHDMf2nWd4eO8Fg4KaIIOlnQWDsdF/FC/ALGVkYECNe/RQRwbIXoClCXh8EpMGYAA5LaAkJHypEN1mGB8jKeNzCXoqxAmwZSTk1EdVAABVZL8BAT01CgAAAABJRU5ErkJggg=="
    },
    "VACUUM_LIQUID": {
        "name": "Liquid Vacuum Field",
        "description": "Sucks liquid materials nearby and releases them upon expiring",
        "meta": {
            "action_type": 1,
            "action_max_uses": 20,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABO0lEQVQ4jWNgoBAwYhOcnyB/z8tWj2Hb4UsMyLR48mYlgga8nOt7b9bqvaexGSwjIWqauOAhiiFM6Daja7784u9OGPvJi9enfx9ovIfVgJdzfe89efEaQ/MNjUlzkQ3p6Og8PT9B/h6KAbhsvqExaa6YhBSGV5BdwsTAwMAQk5CEzcsMYhJSDKpaBljlliyYh3ABjINs+zuHNXNVtQwYrLWFGN45rEHxBrKlGC6AOR2mmYGBgUFVy4ABPSzwukBMQgqumYGBgaE4QIYBPSxwugAGjl59x8DAwMBgocrF0LvhCc4wgCek+Qny8GhEDgMGBgaG29cuMAgdCEnWlWB2Z2BgYKioKDdldahXQjGAgYGB4feBxnsdHZ1wQ5DlYJrRUyNGUkZ2CTpAthkv+H2g8d78BPl76DRBjeQAADdfrFrhxcxYAAAAAElFTkSuQmCC"
    },
    "VACUUM_ENTITIES": {
        "name": "Vacuum Field",
        "description": "Sucks nearby projectiles and creatures into the middle of the field instantaneously",
        "meta": {
            "action_type": 1,
            "action_max_uses": 20,
            "fire_rate_wait": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABLUlEQVQ4jWNgoBAwYhOMzZ//X9XCi+H2iW0MyHRdpDiGegyBpuUv/5/fO3kGNoN5uJUzFk9MRNGDwonNn///y9e7WDXDgKFzbgayS+AMdJsjuW/owtjLv2pcxuUSRmw2R3Lf0DWtzLFmYGBgePH+EsPjGYeOohsCcwkTAwMDg6qFFwM2zQwMDAwSgnoMshl21sguYmBgYLh9YhsDAwMDAxMyBx0kpMQyvHh/iaGitBtDDmYphguQwYI5ixmWrV7I0NFdiiGH0wXLv2pcPt0+5eiL95cYGBgYGIwsFbAajtcFy79qXH4849DRF+8vMXBvYTyKzQCYpfBoxJYGIrlv6C7/qnEZRsPEkdMCSkLClwphAD01YiRlfKkRPRViNQDmEvSMdPvENgb0fEAVAABgYZdona9i7gAAAABJRU5ErkJggg=="
    },
    "SEA_LAVA": {
        "name": "Sea of lava",
        "description": "Summons a large body of lava below the caster",
        "meta": {
            "action_type": 4,
            "action_max_uses": 3,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 140
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAlElEQVQ4jd2SsRXDIAxEv/WyEXkumMQjZhKKPFgJp5IjHMkkbVRxuuPQAfAfVbd1v8JXGgFIWY5G3dbdYq+nGOCmopSFyptQrGtPMxhYYYS93oBa6R8bZrxYMmUZRK30AXsTiSXVxBqeTUOD6IRZjPAO9GRvKqv76RXOMY8JvIxRbjsNwKKE/V2zaqVzfzyXufKLegFIXWGAUM+VLAAAAABJRU5ErkJggg=="
    },
    "SEA_ALCOHOL": {
        "name": "Sea of alcohol",
        "description": "Summons a large body of tasty alcohol below the caster",
        "meta": {
            "action_type": 4,
            "action_max_uses": 3,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 140
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAALGOfPtRkwAAACBjSFJNAAB6JQAAgIMAAPn/AACA6AAAdTAAAOpgAAA6lwAAF2+XqZnUAAAA2klEQVR4nGL8//8/AyUAIICYKNINBAABRLEBAAHEAPJCvRMXij/Q+djEQHyQXoAAArtAR/A3XAGIRuZjE4PxQQAggBhBpqwJYQNLXHnPygCTgPFhFiCLwfgha34xAgQQC7KzkBVi42MTAwgglECE2YgLYJMHCCAmZEmQ6ciKQGxkPjYXAQQQE7Iksv9gbHRD0QFAAKF4AZsNhLwBEEA4wwBmMzZXIasDCCCSYgHdmyAAEEBM2JyFSwzZUJg8QACBExIjIyNK6iIEQJob930D6wUIIEZKszNAgAEAk9l3mQ4AhBkAAAAASUVORK5CYII="
    },
    "SEA_OIL": {
        "name": "Sea of oil",
        "description": "Summons a large body of oil below the caster",
        "meta": {
            "action_type": 4,
            "action_max_uses": 3,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 140
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAmUlEQVQ4jd2SMRLDIBADF8aFJ29ynSdQ+nEu/YTUeRNDRyplLuYM1FFlyTqdYID/QEpb7fGeJwKs6+MrpLRVyz1NHCAA7PuzApSS0Q9xLbCa+HG8wmJrWaPHPS02DrN5Bk2AKtqQUvJtaBOgYVXV9zX0NkAhs1hGBrvZC3YbwO9F6gge4tVsh3rQTJBgX9cIpWTO8x3Gzgl8AIWdS7syF42QAAAAAElFTkSuQmCC"
    },
    "SEA_WATER": {
        "name": "Sea of water",
        "description": "Summons a large body of water below the caster",
        "meta": {
            "action_type": 4,
            "action_max_uses": 3,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 140
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAsElEQVQ4jd1SIRKDMBDcZPCIPqGDx+ER/IBfYPqSmv6iP0DU1+EZnoDIC1K1zCW9JGhOsbd7e0sS4BrV9ZPP4ZzGAkB9ux+Nrp+8xFqPGAAMAAzj0wOA2zeQIOYC2SOe3w9TyVhSqGGtZwOyaf8GSryVpFuXQFQ3bYDduqQNOEwRv2PTpEFqQ+k3kmfAzVoqqQtuIU6gYWl4JOB956LGppwxJOTrKpXbN3w/L1NWnqgfyKhcIqSm0HkAAAAASUVORK5CYII="
    },
    "SEA_ACID": {
        "name": "Sea of acid",
        "description": "Summons a large body of acid below the caster",
        "meta": {
            "action_type": 4,
            "action_max_uses": 3,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 140
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAqUlEQVQ4jd2SoRHDMAxFn325sOyRGbpBUHBoQYcqCA0u6gadwXuYmbikysk5OW5phfylf/7/24L/qGWd8hk+43iAYez3xrJOWWOrJxjAAdxecwaIISEDwSKge4Lvl4frtC1NtLDV8xqIYq2sudfDYewLUgypwJYjr4c6n5yPl1YvqCm0YlTfQJQtV5r30y8cY+4OrIy13NoNfBYJyu1qVQyJ7fp0beYX9QaJEWRAgjto7AAAAABJRU5ErkJggg=="
    },
    "SEA_ACID_GAS": {
        "name": "Sea of flammable gas",
        "description": "Summons a large body of flammable gas below the caster",
        "meta": {
            "action_type": 4,
            "action_max_uses": 3,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 140
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAnklEQVQ4ja1RsRHCMBCTfXS0DMA+6Sk9BANQZACGSEnPPhkgbepQKaf7k7HD8Z2kl/7fBv5VZRy2Hq5Z9+lmTT+FHQ5yotuotuWxad8aex6UOAPA5XreiTIOm2LHEQNA0puWeQUFYg5QjvhZXumka2mjw47LcZrDUbMBbs14ArEG7gEq0FwLVT5HY+3WaKYnsUG/plXLvGJ6vFO7s6M+reRo6ER+u4EAAAAASUVORK5CYII="
    },
    "CLOUD_WATER": {
        "name": "Rain cloud",
        "description": "Creates a watery weather phenomenon",
        "meta": {
            "action_type": 1,
            "action_max_uses": 10,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAlklEQVQ4jdWRSw7EMAhDH1Xv67NwYroJ+ZDMqLvReEWCbIyBX8MAImJruHv/lGTujqRBNNsFZlL2GqKKpMBVJhpgkgxYxFo/3H1xehVyiqWDyImSuvAsclEw79kISMLdqdMB7hN5Fjkg17N8EBE1wK+QZBniDcvOr7Bl0CzPDt7UQ6DYO9afkGeEEU6vD+dcTnt08H94AI8UUrRrwHbnAAAAAElFTkSuQmCC"
    },
    "CLOUD_OIL": {
        "name": "Oil cloud",
        "description": "Creates a rain of oil",
        "meta": {
            "action_type": 1,
            "action_max_uses": 15,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAt0lEQVQ4jd2SwRGDIBBF/2ZytQr7sAitIy4FcIsFsPRBE/ZhFRZALoGwoBlvmck/wcJ/sB+AX4sAIMbYLCzzlIvWBxIRMPPHSNQCSlPXD5TG+7bGGtIAlnmKycTMap4gCczMGvB8jGrzt5MT2BgDALjVprJP60O+kYhARJqs7kfmElKr6wd6t0NNBqeuStYHUhk45656s1QGzJxTBnBprHT0kc7knFM3Vi2Uz7Zva37aul5m8Ad6AUMqfVXAsngiAAAAAElFTkSuQmCC"
    },
    "CLOUD_BLOOD": {
        "name": "Blood cloud",
        "description": "Creates a rain of blood",
        "meta": {
            "action_type": 1,
            "action_max_uses": 3,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 60
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA7ElEQVQ4jdWSsY3DMAxFHw/xDJlBLqIVMoZrNy7pUaQF3F7GyAhWlRkygwteYSuQHRu46y4PEECI+B/kB+FfYGZvb3TORufs+3IxMyOEsOrvGpQigBACIQT2TDKnXKS6tnvXCYCqgvf2BAF4tq3cvLfrNBFBVHVtUIoBYozQtnIeBmtSkhgjTUpzv64tLsYAX9s8SvcsUlVijLPxhhPAo6pW4tJky73r5DwMxjKF5BBv3pubpkNh5lFVNCmJyLyFwJz2X+n7HlgyUFWWsQB+VWd2Q8yrlDXA4Yr5YEbnXgczOmdH/+UhvU3wefwAa12p0JyosIkAAAAASUVORK5CYII="
    },
    "CLOUD_ACID": {
        "name": "Acid cloud",
        "description": "Creates an rain of acid",
        "meta": {
            "action_type": 1,
            "action_max_uses": 8,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABHUlEQVQ4jd2QsUrEQBCG/xHTXNBEEMHqmqtjZWN1kMoieQhNlSbpBFEkb7DTpNG0WtkcWApXpUgaSes9hE0ar5grwobsGUGsxA8Whp1/5p8Z4E8gIl9eUDly834s958XIiJQShl5DekGABDWrpwcTDCdnqPNPdIiO24ksgpiZiRJ0hVSl97RorB2xS8zOny5osgqyI6b3qbNPQprV+y4EWY2picACCpH/DIjbDHm/LC+lDb3KE1TcwKNFgJAZBWk/5gZzIzF27Oh3wWA5WoNf1A8bLKNX2bdemk3fX/E/Se733k+s7BcrTGfWaNNFqcfpI9IAKCU+tZxiN5bKQXjBkmS4PXsrp9gGN8eXfdxUDnyU7Pfs/c4MRzH4n/GBk4Yh1THgFl5AAAAAElFTkSuQmCC"
    },
    "CLOUD_THUNDER": {
        "name": "Thundercloud",
        "description": "Creates a stormy cloud",
        "meta": {
            "action_type": 1,
            "action_max_uses": 5,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA3ElEQVQ4ja1QsQ3CQAw8I6ahQKL4AZLUpGGQ3yNN3oOkSKgJIyAaFmCQo8HRJ3lEgjjppdfZd/YZ+AdIzl5/zGmPJEIIo3rSIBbF9ZTJzMBE1tQfc4YQRibGxQZbALiWBfNzL0aqKlCUsr90JCmqisx7ERH0ABUYejfTe3jvh3/WXcQ4VUW80Qhx5tRBp5njfjHRtSwGcnc4JAc9bjcAQBz3I1zdVCR5au/d1+YpTu29I0lXN9VqMd8wk1Xi8ByvbDHCc4GRq5tqmtc2cXVTLTJJbRQPWC3+6Yhr8QLubNfkjKAxcQAAAABJRU5ErkJggg=="
    },
    "ELECTRIC_CHARGE": {
        "name": "Electric charge",
        "description": "Gives a projectile an electric charge, that it will release on impact",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 8
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAbUlEQVR4nGNgoBYIb3+OVZwFq+jq458ZGBhWYpNiIt5snBpCHHgYGBjWHPhClAa48SsrJYnSADEeD0DxNLLrIf6GgFBLXuw2YDUezTOMuKyG24BsPLoNBI3HqQFPWGHXgCcq8DkJV1TgcxIVAADaQyN51kYVCQAAAABJRU5ErkJggg=="
    },
    "MATTER_EATER": {
        "name": "Matter eater",
        "description": "Makes a projectile eat the environment as it flies",
        "meta": {
            "action_type": 2,
            "action_max_uses": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAoUlEQVQ4jaWTuw3DMAxEj17BhdvsoAwRQIXn8ESaI0UAD5HskDaFZzhXNhhKIqTkGkH8PEH8CBylGLg+314IBtfboAwwTyP/Atw/m+j77XpxAWfwPI20yYdSDASAUj2yhBQDl8erCrKQYqAnC2kCkCQAiIhYyKADaqen7i8A362uDtJReWuzdtHOUvV1QlMb9Tz8tAu1YWoC9O5BBuh9HQB2DetWKThJ8RwAAAAASUVORK5CYII="
    },
    "FREEZE": {
        "name": "Freeze charge",
        "description": "Gives a projectile a frozen charge, that it will release on impact",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAr0lEQVR4nLWPMRKCMBBFfywCBaTwCCmpAuegp/cy3sKeyyAVJYeIFEgTi3XiJlFmnNFtsrPz3t8N8GUJei6j7acFQFcVAIyW47x5qJ+WripORgE40Mho6VHPUU+0nz+FRuXkjPNmtGxU7rVzeyQgEHgk70nm85fAj+aRAOoyiz9NNdiV04Nd6zITImCCk6K8lI4FvhrA9XZP4wIhyov8jyelZ+yVc24feLPhx8L/6wEchShYugcbZQAAAABJRU5ErkJggg=="
    },
    "HITFX_BURNING_CRITICAL_HIT": {
        "name": "Critical on burning",
        "description": "Makes a projectile always do a critical hit on burning enemies",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAASpJREFUeNpi+P//PwMIf42X/w9jk4KZGKCA2d2TgRzAhMz5uTzzP4hmZGRkuHTp0n8QBrFxYbBakDOQNYMAR9QMsOzixYvBYk+fPmU4fvggg7KsNEPfjDmMMHVwL4A0/3eUZwBhZBAbG8sI0wwCYgoqcLmLFy+CDWch5EeYZktbe4agb98Ygurq/p9UVWXgXLuWgUFPDzUM0EFRRsp/mGZpaWmwGEiz+e3bDGpNTYxYw4A9cjojKIBAmu8+fsqwcet2lPAAaT598iRD9M6djPAwAGkABRwIg9gnNrSjaIYBPaCTQZpRAMwFyM729/ZEEQQF2C2g35e6u4PFYTRYL7IBIJvRNSMDmEYUy2EGYLOZaANAmmEhTgrA8AIhgM0FjKQYQDAzkQMAAgwAJw/Zu9OkwM4AAAAASUVORK5CYII="
    },
    "HITFX_CRITICAL_WATER": {
        "name": "Critical on wet (water) enemies",
        "description": "Makes a projectile always do a critical hit on wet (water) enemies",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAUJJREFUeNpi+P//PwMMyyQt+o/MJwYzMSABUQVNBlIBCzJHS14Uzr506dJ/EK2vr8+ISzPIBYwgAgSMms/8Bxlw7eFrhnO1JoxAwLB48WKw5NOnTxmOHz7IoCwrzdA3Yw4jsgFM6KYiuyI2NpYRphkExBRU4HIXL178D/cCzHYQsNOTB/OBTLBNMM2WtvYMQd++MQTV1f0/qarKwLl2LQODnh6qC0CakV1RlJHyH6ZZWloaLAbSbH77NoNaUxPYAowwAIElcfKMxZmp/+8+fsqwcet2sEJYeIA0nz55kiF6506wXrAXQAEGos5DXZLL346iGQb0gE4+vWQJZlQgA5Cz/b09UQRBAXYL6Pel7u5gcRgN1otswIkN7RiakQFMI4rlMAOw2Uy0ASDNsBAnBWB4gRDA5gJGUgzABpgYKAQAAQYAav7Yh4Kjkz8AAAAASUVORK5CYII="
    },
    "HITFX_CRITICAL_OIL": {
        "name": "Critical on oiled enemies",
        "description": "Makes a projectile always do a critical hit on oiled enemies",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAR1JREFUeNpi/P//PwMlgAmZ01HkS7JpLMgcGSlRBkZGRgaQqy5dugQ2TF9fnxGXZpA6RpgXQLaDDHjy7DWIy1jZv4Vh8eLFYMmnT58yHD98kEFZVpqhb8YcRmQDmNBNBRkCA7GxsYwwzSAgpqACl7t48eJ/eBjAbAcBZTkwDQ8LmGZLW3uGoG/fGG7V1f0HuYxz7VrMQIRqhruiKCPlP0yztLQ0WOykqiqD+e3bDGpNTYzIgcgI9Dvc1tjS+YwgzXcfP2XYuHU7IxCDwwOm+fTJk6ghiQ5ObGj/7+/tiSIBMgDk76Xu7mAM14tuAMhmdM0gjSC/wzTiNACbzcgAphHF9TADsNlMtAEgzbAQJwVgDQNSXcBI1dxIDgAIMACet7j2ZHTdrgAAAABJRU5ErkJggg=="
    },
    "HITFX_CRITICAL_BLOOD": {
        "name": "Critical on bloody enemies",
        "description": "Makes a projectile always do a critical hit on bloody enemies",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAATFJREFUeNpi/P//PwMlgAmZc8Dd/T8Qk2+AiqAgnH3p0qX/IMzIyMiAC4MAI8wLINtBBtx5/x4s7rhrF8PixYvBkk+fPmU4fvggg7KsNEPfjDmMMEtAepnQnYTsitjYWEaYZhAQU1CBy128ePE/3Asw20GALywMbDhMIUyzpa09Q9C3bwy36ur+g1zGuXYtZhhANcNdUZSR8h+mWVpaGix2UlWVwfz2bQa1piZGeBhAQx7uCtmVKxlBmu8+fsqwcet2sEJYeIA0nz55kiF6506wXhaQICjAQIbBXHJiQ/v/9tmH4JphQE9Pj+H0kiWogYaekEA2+3t7ogiCAgzk96XAsALxYTRYL7IBIJvRNSMDmEYUy2EGYLOZaANAmmEhTgrA8AIhgM0FjFTNjeQAgAADANNFsTOooQNiAAAAAElFTkSuQmCC"
    },
    "HITFX_TOXIC_CHARM": {
        "name": "Charm on toxic sludge",
        "description": "Makes a projectile charm creatures covered in toxic sludge",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA/ElEQVQ4ja2SMQrCMBSG/wqeoYMdWsmUwfYMQhy7iqvQAyheQ3uA7h6ihQreQASnghnaE/QAzyklNqlW8F/y/kDe+97LA/6p5MRJ95QkpJ82TXTDo2kX54xRI+XXJG8Jojnv4lVVOY/nE42UAAAny5yP+GkZ0kVuKC3Dt0o5Y4P4BkGfQpHovt+So6qrh9zb4nw9YLe8Gcg5Y8SDADPftxNwb2ul0GkG56JmcJEbqoWgo+sSANRCUP8H9LlYMe9ti30cGwS2nzCGeG9brBeLDlOp7wcJFAUPAgDAzPfRSAmvKIb3QPWk95ozRrUQ9G0PjCSf7myFftZoorF6ATIdgRC0wXA7AAAAAElFTkSuQmCC"
    },
    "HITFX_EXPLOSION_SLIME": {
        "name": "Explosion on slimy enemies",
        "description": "Makes a projectile explode upon collision with creatures covered in slime",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA50lEQVQ4jWNgQAITdBr+M5AImBgYGBj+///P8P//fwZhXiE4myQDYEBZRBHO/tzOTZQpcAOWWE3+j0zzVn5lxKcRZgETugTMFd8P+//HpgEmh2LBYstJ/4/5bv5/zHfz/x8Tnv9fbDnpP7Ih+LzDgswxcjZBcQWyId8PM/zntN2I4S1GBgZILCyxmvwfptFykw/jlw6e/ywJNQiVd07AmX+O7IGHEYqJn9u5/7PYuCAEVCwwDMDmCqzg+2H//9+ft0MwUoCiBy7cZqwGQDFOjbgAzEB8UUk/QMjpGCkRHfw5sod6rsEGAPBsgcZk/VWyAAAAAElFTkSuQmCC"
    },
    "HITFX_EXPLOSION_SLIME_GIGA": {
        "name": "Giant explosion on slimy enemies",
        "description": "Makes a projectile explode powerfully upon collision with creatures covered in slime",
        "meta": {
            "action_type": 2,
            "action_max_uses": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 200
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABWElEQVQ4jY2TsU/CUBDGf68SkqZ0ERZGQifjImFoDEzgKv41Jt0cTLqauDo4OJOwmm7twB+BYWRxa0iXJs9B7vUVSvSme3fvvvvue/fAspfrJ82RFeniJGbnHACtNVpruv6l8c9ZHntaissswbGTw96g8XIee1p8P9orAIIQP9orJQw+bl/1sDfg63vLw30EQGsyhyCEzZoySxCAYhdridUY2Cxak3kVDEITK3axLt+fTeoCIPjsaikczcYsl32u7ramEMDptM3Z6bQpswQ/2qsag9Fs3KiFzUKoixYnGgDcPL7VuhvbrAFwpysloRaAUoo89irR+BWuESQIKVLMOyuonssI11RosbAZGOdfIIcR7DGUnTdre3h7m7aM5PYjJXfLLKkAinSh3enqd0kAtx+pPPZ0bR8OYAJiTOgLkH2WNRZ2RbrQ0uTP4uMm9s+sgTSBnQM5bvgDdKnBacbkxXIAAAAASUVORK5CYII="
    },
    "HITFX_EXPLOSION_ALCOHOL": {
        "name": "Explosion on drunk enemies",
        "description": "Makes a projectile explode upon collision with creatures covered in alcohol",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA0UlEQVQ4jWNggIK56Sr/GcgATDCGsjgjOfoRBshKiqNIfG7nJspFTAwMEOc/fv6S4e50G7gm3sqveJ0Es4CJgYGBYQGLJEMPuzpDDYssXMH3w/7/sWmAyaFYMDdd5f+BOtX/yC5ANgSfd+BhcPflf4bHz19i2P79sP9/FhsXDBfBAE5/fm7n/s+SUIMQuHMCzvxzZA88jOAu6InX/Q+jYbZiA5y2GxkJBTDc6d+ft0MwmpewOherAVCMUyMuADMQX1TSDxByOhM+SQYGSJTRFAAAd6p4DlVMTDUAAAAASUVORK5CYII="
    },
    "HITFX_EXPLOSION_ALCOHOL_GIGA": {
        "name": "Giant explosion on drunk enemies",
        "description": "Makes a projectile explode powerfully upon collision with creatures covered in alcohol",
        "meta": {
            "action_type": 2,
            "action_max_uses": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 200
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABQUlEQVQ4jY1TMW6EMBAcR5YidEJXIgoq/AgXXEfPF9JFeQJ9JF4R6brU19NBwSO4mlyNkBXJklMcu2cIKNnKa8/OzC4LMMfHq3LYCNMUm/f09kRJGok93CLG6uCo2LY1mCCJo13wWB0cncNyuispjbCchCD7aSSQxBHSt1YQWGY5oDTQd7BtDSIwQ+XoTgLAWcY4Ph8RypDVZZY/rCgNtDVklsMM2tnzO79LAHixA9LvLyQ2wqdfrPSScM5llsO2NcJyEpIA15sDcNufHpHN1mkWv0bPn81T5+g7AEBwunAdOxirg/Ntou+2SZSGacC7IaiY+9xT91z4DvjwL5K5Bb+NxQwW/XtgzpVGEJeCsLatHwSmKVxwutyXBEAQl4LnspoBkXCQfSLyc1pjcmeawpHIn8VrEf/PXJBske2RrAV/AIgQtgBliQ8nAAAAAElFTkSuQmCC"
    },
    "ROCKET_DOWNWARDS": {
        "name": "Downwards bolt bundle",
        "description": "Makes a projectile separate into a bundle of 5 explosive bolts as soon as it moves downwards",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 25,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAxElEQVQ4jWNgoAWo92uaS4wYAwMDAxMxhuDSjBX0Rk04CtME0wijYXLIgAVdQJpN1oqBgYGBj4dPAyYGY8PkcIIVCWv/o7N7oyYcxSaOFZyuvPAfl0EwGqYGp2ZkxciBh2woslp4LCjYyMMNU5ZUZmBgYGDQFNJOQhdDV8vEwMDA8Hrr+/8iXgIYropYEMyIy8//////j8J5vfX9f5hhuLwGk0NWjwGQJdADDKcmZAXITkPXgNdmbArQ+egWEAQkayAHAADzOp7/XDCwGAAAAABJRU5ErkJggg=="
    },
    "ROCKET_OCTAGON": {
        "name": "Octagonal bolt bundle",
        "description": "Makes a projectile launch 8 magical bolts if it moves slowly enough",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAsUlEQVQ4jWNgoCV4vfX9//////8n24D/////f731PV4DmPDZyMjIyEiSi7DZeLrywn988lgBTBOyZmQ2UWBFwtobKxLW3oCxcaljYmBA+O311vf/YTZFLAjWuP7u6qt6v6ZD6C5ADgsWBgYGBhEvAYY32z4wiHoLMsIU331+9+b1d1dfwQyDiSvYyMPVE/QCuleIBtgCj6hARI9ndE0EoxFdAbpikpM20QkHF6A4MxEDACXlu01+ZuEiAAAAAElFTkSuQmCC"
    },
    "FIZZLE": {
        "name": "Fizzle",
        "description": "Gives a spell a small probability of short-circuiting",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -10,
            "speed_multiplier": 1.2,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAt0lEQVQ4jbVTsQ3CMBA8I9ago0vBNN6BTBCLFim6TGB2SJkB0pGeygW7PA2WXo4NIREnWfK//3T3bxv4F0QkrCbfh+G5muzJ8Zf6XSY36UC34kn7USC1HmNPjp60Oj+bkSaLSIhKnrRfByoi4b2u2qJWTduJ2Kt9f+u6x7lpLAALALVzfWzhVFUHY8yx6EL3qx0sfg+amA7Jk23pDLmigkCbu0KTcTEBQO3cZZH1EjY954hNH2opXsdKgJDJCHpwAAAAAElFTkSuQmCC"
    },
    "BOUNCE_EXPLOSION": {
        "name": "Explosive bounce",
        "description": "Makes a projectile explode as it bounces",
        "meta": {
            "action_type": 2,
            "bounces": 1,
            "fire_rate_wait": 25,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAvUlEQVQ4jWNgGHLgPxTA+Ey4JCiygRRxogwhy2UwTYQ0MyFzPrdzUx4G2Fzx/bA/aQbDNKA7H5tBjLg0M6hYIATvnGBgYGBg4LTdiKGesby8HMPUBp8bGAY0bNHA6lqWM2fOYAg2MJgwNPicQDWEgYEBm1omDBGoCxq2aECcDnU+AwMDwwaXExhqMQzY4HKCwafhC6qBWzQYzpw5wxCwxwJdOSQQnZ2dsUaTiYkJQ43AFKwaGRgYGPbu3csIAEIXZ9AexbZmAAAAAElFTkSuQmCC"
    },
    "BOUNCE_SPARK": {
        "name": "Bubbly bounce",
        "description": "Makes a projectile shoot bubble sparks as it bounces",
        "meta": {
            "action_type": 2,
            "bounces": 1,
            "fire_rate_wait": 8,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA+ElEQVQ4je2QrU7DYBSGn2/pkrW6liy7hjrMRO22ZKYJFRMNHlmBwXAB85tBbEkFCGqbOlR7C5D5KkRHYMnBsKbL1wYa7B73vucv74EznZEfjrrXVvjXhS6+hjldiYiIOV1VA52Gg7SUIC2rJSIiF3fvJwvrGHVhOx6ToaE1LRcD4rHPFmT/fK3qtV5dFHlEvDsAYM3W2I6HNVszH/WZDA1sx/tbjGOUx9dPCdKy0k0xVBiGmrkxb1kuBsxHfZ7evrh5+MDf3zceNLIs08zCiojHPgDx7kCRR2Sl3gegXNdt/O6LdYXteBR5xGW5bY2sANqW/EaSJOob2FGC5DhxtbwAAAAASUVORK5CYII="
    },
    "BOUNCE_LASER": {
        "name": "Laser bounce",
        "description": "Makes a projectile release a bundle of concentrated light as it bounces",
        "meta": {
            "action_type": 2,
            "bounces": 1,
            "fire_rate_wait": 12,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABBUlEQVQ4jbWTr04DMRzHP3dZpskpxAwPMEE4hThzgTdgigRz73D3BnsDzBYwC2Yes5zYxNTPnVnCkplpgt1MEaxNb205DD/Xfv+2TeE/Rymlujjxb+DqOPWa2sZxCABYN4vOlsYgOo1tsmk+Wsk2L3iEcxMttkXeBiGTkFjjsb0AeHq97rz55WHimi4PE0foe8axjMIB56A20K3+0q5FHstIdYmjsiwd4OJhx+3wjqxfsDpOWTcLvuZX3sCeiJjF4PGT/SwhJSW7KQDI+gUM4X2+xebqaT3jfpYAICJUVWX2X96eTYDTwNvrNPovXG7uf9IlcTgRQJ7nrXswR0lTb209dV1H34xmkgY6CN4QAAAAAElFTkSuQmCC"
    },
    "BOUNCE_LASER_EMITTER": {
        "name": "Plasma Beam Bounce",
        "description": "A projectile launches a plasma beam upon bouncing",
        "meta": {
            "action_type": 2,
            "bounces": 1,
            "fire_rate_wait": 12,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA/UlEQVQ4jZWSPWoCQRTHfytbJyzYCDnEENbWZrHwCgopPMC2a5tCkjp30CsEDANi6xQieIoEYnKBl2I/mOzMjvpv5uO9+b3/mxkIaHMWCcWD2pxFREQuQeKuwP4AqGoMqHerM6nkANoBgPH9ZWADiCq1IXYBO88B2KA2RETEPnSV6hfoctSpx+eT1ADfvVyt5bY8vNyGAd5nfHhafYbWtuKiKJwKd5MpQ1XOhwqYT/u/g6PXSWyMaRbfozeSXU7KAtQrUP0FBfod7FxvC8kuB8pE/bJo9uf5+qsu4Djo6g3g46ccZ4Nj3xhDgusgAsiy7F9/TStp6rVdS2sd/QE4oYftHSnYiQAAAABJRU5ErkJggg=="
    },
    "BOUNCE_LARPA": {
        "name": "Larpa Bounce",
        "description": "A projectile will launch a copy of itself when it bounces",
        "meta": {
            "action_type": 2,
            "bounces": 1,
            "fire_rate_wait": 32,
            "speed_multiplier": 1,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA4klEQVQ4jbWSIQ7CQBBF3xI0Sa9C65ANCYKLcIIFhWUVXKAWgwSBqQPFImoJB0DBDRhEKWnpbiEhfLPZmfn/z+wO/BMiIp9qWr+aNArcjvWYPFETeE8AbJP99x2oJ8oi2flQcS7XeUd4FynIZZKzA59IE9kpoPtz79c15Zz4aQ+Wo50UZ5Oz0lrXksYYdbUiQZjvwjbZk3XWToG2tfZ1iVpD7H3DcrSTIMxjQQgDemSrNeVa5wj2vgEgOU0rW2gmi5dBrQPfbGY1RjPLL92L0x1AAcRxXHmHYpQoirxEgDRN1QOoPnaCgzwlyAAAAABJRU5ErkJggg=="
    },
    "FIREBALL_RAY": {
        "name": "Fireball thrower",
        "description": "Makes a projectile cast fireballs in random directions",
        "meta": {
            "action_type": 2,
            "action_max_uses": 16,
            "speed_multiplier": 1,
            "action_mana_drain": 110
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA1klEQVQ4jbVRsQ3CMBA8YAHaKDUbuKSgyQLZgB4pQtnBFWOkTRagYARaS6GMlZIRjgJ/5DhGWEJcZb/P/3d/wL9htOJPRA4tv/KMViTJkOzeKr8ZSfZFNvHWALC5WcB2QF5+VGK0IvISsB1213EVV+EmBBZqACDJpH2IHZHPoa05tGmfg8l0n+uFSq/h0ovDszk328P+PhXy8jIj2A6P4+l97ouMRiuGaYgFSUJ4M0t+MVyUI1axeBeQRrM9eIkkpyDw0/DvyYnEiGIzqUmM0BdZuoJUvAAnV+s4GLaPZAAAAABJRU5ErkJggg=="
    },
    "LIGHTNING_RAY": {
        "name": "Lightning thrower",
        "description": "Makes a projectile cast lightning in random directions",
        "meta": {
            "action_type": 2,
            "action_max_uses": 16,
            "speed_multiplier": 1,
            "action_mana_drain": 110
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAD2SURBVHjaYvj//z/D/////kNoTNx+7RNeOYAAQlKEzRBUMWTDYGyAAEKTxDQEJG636ep/TJdA1AIEEF4bQRikGZe3QDRAAGFIwGzE5myIOKolAAHEQCjwYIaiewHGBwggRrApOAGyJCMjqjiEDxBAjCAngTgd1z//r9DkYbDffJ3BU0UWrAzEh4GO61/g7ApNXkaI2Of/AAEEDzzsTvwHxzB5WPjAwgMggJC8gHAW3HQkALMVHQAEEE4XoEcfrhQJEEAEnI9pEDofIIBwROM/vHGPbAhAADEQzkT//mPPBxBxgADCqhlb8sVlKEAAkZR9sakBCDAADkCqRgMvy1sAAAAASUVORK5CYII="
    },
    "TENTACLE_RAY": {
        "name": "Tentacler",
        "description": "Makes a projectile cast tentacles in random directions",
        "meta": {
            "action_type": 2,
            "action_max_uses": 16,
            "speed_multiplier": 1,
            "action_mana_drain": 110
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAADoSURBVHjaYvj//z8DKbi1ZeF/ZD5AALEwEABZGR3/ZWSl4Pwnj5+hyAMEEAsxmpE1TZtRwYisBiCACLoAmyZkABBATAxkAJDLYGyAAGLCJkiMi2DqAQKIERSSIA66M9EDr6o6jhGbSwACiDEzvR2sEKagrXURTpcgGwJSBwpcgABiQRZAjiZk29G9iexagACCewGbjeiGoBsO4gMEENgAbADdK+gJCOYSgABiwRaA2DThUgMQQEzIUYLNdphGZDFk9QABBA8DbOkd2VZ0l8L4AAHEgp4w8DkZWxQDBBDOQCSUhGEWAAQYAAFBtvdT39irAAAAAElFTkSuQmCC"
    },
    "LASER_EMITTER_RAY": {
        "name": "Plasma Beam Thrower",
        "description": "A projectile fires plasma beams in all directions!",
        "meta": {
            "action_type": 2,
            "action_max_uses": 16,
            "speed_multiplier": 1,
            "action_mana_drain": 110
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABAUlEQVQ4jY1TMRKCQAxc/IAWjBVWdNow+gZ9hAX+BEdnzn9YaOcHsDkegGOjnZWdWOALYsEEwnmnUt0l2d3bkAB/fmlJlJZEZrxjBpT+LEpLomn3XykA49WlRaI0ERGRjfwnwXh1sYJbdtifLAriXSEJmFTpqo6ownimx00GHLIrHrfzsx9Gfp4MMVlfa7E8GQIAji9g1vO81iuYmRVZPYh3hYx/7QUDJZnSFYnZXADw5IV9A8B9O/dlbrDYP/ncDyP/tBw1WKWbJvFzWVVakAJOK9K3/ANMYrPxATbvDHLNRG3DVLDFrCRp6R5V2xjLQarBtk2T5Ga+3k7XmprFrtwbAfZmNHxi2TsAAAAASUVORK5CYII="
    },
    "FIREBALL_RAY_LINE": {
        "name": "Two-way fireball thrower",
        "description": "Makes a projectile fire small fireballs perpendicular to its trajectory",
        "meta": {
            "action_type": 2,
            "action_max_uses": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 130
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAgUlEQVQ4ja3SsQ2AIBAF0MMt3ANHcB417ICtQ9Cyzxlbekf4VqgRI+HwlxAehPtEf4WtBgCw1RABW98CwdchbPWJiIAnVIXcIIPgjQjY3eQQI0XiebZ6eNtvcoBSSq1zNxLRUnxzsha8fMREV9k+oVwHYk/EY46NTV5Q0r6qf/g9B02jfUGepl+RAAAAAElFTkSuQmCC"
    },
    "FIREBALL_RAY_ENEMY": {
        "name": "Personal fireball thrower",
        "description": "Makes a projectile turn the creatures it hits into living fireball throwers",
        "meta": {
            "action_type": 2,
            "action_max_uses": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABGklEQVQ4jZWTr04DQRDGvyO8QB3bIuqKQpAzlYitp9W9IPAYmnAvcCGpaaoREJIack+Aqauoq+GSJam5hHN9AvIhLnNc9+66ZZI18+e3MzvfAg5LIp9Of1MSADCNKzGmMUnSaEUkkU+SLCeWgTa8km+0ohDrOjFa7fmYxrR9OTWNabebRP69HCk+NG4BkuJttuQ2W3L1OadAjrJysQty4oLd3o2R7TZ4nEz3upRRnICX5zcs3l/xNJ0AAHrhGugM0QtCGK14CuQv/XPdyROCEN75yEvwgN1NMDtrXeKq3y2AX4M2JPfi49uD0YrFFsjKFmR250MKqCn+ry2I2bqok3WjiWTtLkXGBwUlt1Uki7+PdJQq6/zydyT+C9MNADGRUnpaAAAAAElFTkSuQmCC"
    },
    "LIGHTNING_RAY_ENEMY": {
        "name": "Personal lightning caster",
        "description": "Makes a projectile turn the creatures it hits into living thunderstorms",
        "meta": {
            "action_type": 2,
            "action_max_uses": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAGUSURBVHjaYmAAgv////1nwAHar33CKnejzUQIJAcQQHBF2AwBif1/slYI3TCQGIh921VSCCCAQCYBFf5HMQRkOrImu01X/8M0g+SACoVgagECiAFoCshEsCHYXAHSjOIqqO0wwwECiBFqKoN6XCVDxydXsMT2O48ZDvlpMwLF82EaNarOTAQZdtBXk4GRkYkRJg4QQOgBA3YNlJ1//8WB/yB8/Pqk/8iGIYcHQADhMgiuGZshyF4FCCAmGAcWiOh+TkiJZXjx/hJDRWk3wuan60HeA7MBAogJ5B+YISA2yO/IBiyYs5hh2eqFDB3dpQyw8GGQDmJI01n4v0KThwEggJhAgnfcpBlBHFgYgALs57ySApDNIGBkqQA3cO5kF8ab7aYMs67EM4IsBAggSDQCXQDxwn+U6AP5GeZ3bAEJAgABBNcMMgjkAvSUCQsTmGb0MAIIIKwIphnkHZAGZD66IQABxIAvEyGnTpgr0ZM9QABhRWBbgbaBNGAYCk724EwG5gMEEM7sCwsPdAAyFKQGJg8QYAAulFt4JRO0WQAAAABJRU5ErkJggg=="
    },
    "TENTACLE_RAY_ENEMY": {
        "name": "Personal tentacler",
        "description": "Makes creatures hit by a projectile grow tentacles in a chaotic manner",
        "meta": {
            "action_type": 2,
            "action_max_uses": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAGSSURBVHjaYmAgAG60maDwW1sW/kcWBwggRhhHo+oMVgMy09v/y8hKwflPHj9jmFarxsggHcRwx02KASCAmECa1StPM/x/shbDVphmkCYYzpdfC9bM8HQdg+ru5wwAAcRw21USrPn///8YzgUZAHMyDIDUgvTAAEAAwW0EG4LkCqh4PgzDNIPUggyGqQEIIBTbYAbBNN9/ceA/CB+/Puk/zBBk9SAaIIAwOWiaCRkCEEBM6KGM7ueElFiGF+8vMVSUdqN4F6YOIICYkDWCMCikkcGCOYsZlq1eyNDRXQrmT3wY/B8cC1AAEEAs02dWMiJ7ARTHjDJnGG4wlDC8D4ybICGox2BkqYBiaFvrIrh6gAACRwk8Fv7/x4gFmN+RwwBkIcxSgACCByDMIGQvweSgMZSPLaUCBBATzAugVAVLzugBCQJr/+VOgEUxspcBAogRV3oHh8eMCkYggGsqujeJUWXXM3Ayzmq+9R9kOUAAMeJKC6DAvJOYA0nvOCwB5QuAAGLEl43RcyiyJSDNIHmAAAMALk4OMtbrhKwAAAAASUVORK5CYII="
    },
    "GRAVITY_FIELD_ENEMY": {
        "name": "Personal gravity field",
        "description": "Makes creatures hit by a projectile gain a temporary gravity well that draws projectiles in",
        "meta": {
            "action_type": 2,
            "action_max_uses": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 110
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABGklEQVQ4jWNgoBAw4pJ4O+HndGS+cAF7JlEGwDTuv/sARdxRWQGrQSzommEaz948cx/NbEWYGmRDWNA1wzQ+fPv4Lor2mwiDkA1BcQEMPHz7+G7rVos1DAwMDC/eX2Jo9H1gxMDAwCAvLKuMrpYR3XZkzTAAM0SIX0xJXlhW2VjdRNFRWYFBuIA9kwmbC2AgISWW4cX7SwwVpd041eA1YMGcxQzLVi9k6OguJc2ATJtNRi/eX2JgYGBgMLJUwGcHxADhAvZMR2UFBmN1E0V5YVllIX4xpUbfB0Yv3l9iWJkpb8TAwMCAzf8MDDhiARbak2O+hQjxiylNjvkWgi0GGBjQUiK+hGSsbqLIwABJkcgJieKkTHFmohgAAHrlgPg7ZNY6AAAAAElFTkSuQmCC"
    },
    "CURSE": {
        "name": "Venomous Curse",
        "description": "Imbues a projectile with a curse that makes the target hit by the projectile to waste away",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABRElEQVQ4jWNgoBAwYhNsDgzebSZv/A7GP/XwrFDt+rWuRBnQHBi828DLrRZd/MK2Xc3YDGEmRjMDAwODhKryfi0W9vn7b1xfjCzOhE2zlizHcWRFML6Bl1ttc2DwbqwGIPv52uMfljBNWrIcx689/mGJTR2KAegApglZMzYAN+DUw7NC+BTiUgcPxP03ri/WYmGfL6GqvJ+BgYFB9fVsc+Fv52SEv52Tecdt/JSBAXdMoIDNs2da3Ggzyb//4sD/+y8O/D9+fdL/G20m+Ztnz7TA6wUYUH0925w9qWcCjC8hqMfwPjBugurr2eZEGYAMElJiGV68v8RQUdqNUw1eAxbMWcywbPVCho7uUuIN0Kg6M/HnvJKCF+8vMTAwMDAYWSrgswO7CzSqzkwUXL+o4MX7SwyWpx8W4DUBH7jRZpKPTGMDAPaPgZ3XegQzAAAAAElFTkSuQmCC"
    },
    "CURSE_WITHER_PROJECTILE": {
        "name": "Weakening Curse - Projectiles",
        "description": "Target hit by a projectile takes 25% extra projectile damage for a time",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABgklEQVQ4jaWRP0gCYRjGn/4RtOkkcaHBDVdISIh6ETkFDS5CQ0sR0dBiGiiEKCEXEuigNQnRPyGhBgeXwEFbOjMHEcEbDi7hOG5SaBCabBDllPMQepaX7/ne9/c9fC/wT02pmW4Pkzeaba3+uVkv67M34e2JAG4Pk9/ZsIRH/bePKqMGmZlkGADIJUNhbnH1nisX0kp/Vmu4Q/hZ1+YuSFyCRwiAgwaYvDLJAGA021od4pQlCRK8yIMkSFCm3jCAXt0LsY2frRfVBACwIMZpSQRA+Fle5AEAHFygTBZw31VIpRMawJkqoFkv61c2LAMQAEhi704qAYBj0KcEDG1B+Q9OIWLv++/LF5/A+E0MKZXJObio1SvIxa4gF7ts47rLRa3eVCbnUOufHjWcQsQ+fxRP9M8G3Rra7oOEMpEmQKnD433I7RrOA7GxPZqAh9s0nl8fcRULTA6ggpXk753fJ7drAIB12qT1hnoCKlhJ6rJPPrldA/3V9GkStMRFrV5lVdMfjXqQeJRQ6vYAAAAASUVORK5CYII="
    },
    "CURSE_WITHER_EXPLOSION": {
        "name": "Weakening Curse - Explosives",
        "description": "Target hit by a projectile takes 25% extra explosion damage for a time",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABuklEQVQ4jaVQPUhCYRQ9/RG1aQ3RUNGiSYSEpM/FhgobGnRp6Af7oU10KIjCwAQJasiahOyfHMIaHIoMaog+sDeYSb2GejyheFEkLUUtX9OzRz4l6Czfdw/nnnvuBf6JIiXS5vTF6pvbXqVaSMXVAf12R93IbY6+VF6kQxrqSvQfW816D/CV5adH78jYSv8x4MkZViwvpGa7LkLMRw4CAHZdhHywBFaz3mNz+mJ5d7E5fbFgOGq6Gqqi6ZCGPl86KP28p+mQhkp8MBw1/TbJJpB2Pu9aZ97OXlCtnQEAVBgYSLxcp7iChBp3D164WQDAB0tg10VIvuTZIwqpuLrJrIfxaZbsYYYBgG62mxxULjC4/mkQUnG13KBE+nDxk62yWt1aRVNvHwBYeK/xPfFobSy+TAmq9gcAODxP+PaXPZ350gAAguGoifMbXLx4SnnxlJKbJcr5Da5gOGpS0ufcwMJ7jeXDC4vZe6hakLENLlp4r/FPBnI4RgcgZpKYnJjPqylosL6yhZ3dDczNT/zdQDvFBj5Xx91iJgkAaGUaCs1QTqCdYgOq/U23mEmCuRDcBR0KgfMbXPJXCd8I3KuVT0ipSAAAAABJRU5ErkJggg=="
    },
    "CURSE_WITHER_MELEE": {
        "name": "Weakening Curse - Melee",
        "description": "Target hit by a projectile takes 25% extra melee damage for a time",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABP0lEQVQ4jWNgoBAwYhMMzG3eLa9j9g7Gf3jllND6ybWuRBkQmNu828PKoBZdfMexC83YDGEmRnNahM/xjx8/RbNKac2/cWr/YqwG4NLMF+Zz/NjKLZYMDAwMKrIS+9ENYYIxkP3M/2DtcZjmT6sgmrGpQzEAGXxUCLaMU96FoRkbgBvw8MopIRj707mVxxfddbOEuQQZIKtjYECLBeRwsL/faA4TP6hYf5KBAXdMoICZyzdb3Ggzyb//4sD/+y8O/D9+fdL/G20m+TOXb7bA6wUYsL/faM6e1DMBxpcQ1GN4Hxg3AdlFeA1ABgkpsQwv3l9iqCjtxqkGrwEL5ixmWLZ6IUNHdynxBmhUnZn4c15JwYv3lxgYGBgYjCwV8NmB3QUaVWcmCq5fVPDi/SUGy9MPC/CagA/caDPJR6axAQCkF4myPXhFXAAAAABJRU5ErkJggg=="
    },
    "CURSE_WITHER_ELECTRICITY": {
        "name": "Weakening Curse - Electricity",
        "description": "Target hit by a projectile takes 25% extra electricity damage for a time",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABg0lEQVQ4jWNgoBAwYhMMzG3eLa9j9g7Gf3jllND6ybWuRBkQmNu828PKoJaBgYFhz2+x4y6srywZGBgYdhy70IzNECZ0zSymbi4wflC0GQMDAwNDWoTPcRZTN5fA3ObdOF2AbDNfmM/xT6u2WMI0z1oBYWNzCQuMAfHzLwYGBgaGdUtPMQRF+xyPYGJgWPEP1UbksMHwAszfQdFmDDDNn1ZtseQL8zmOrg7DgIdXTgnB/P1p1RbLFf8gLoG5CF0dDDDDGDdO7V/MKqU1X/bTp2gGBgYGiy3RVzKzGz14NkdfYRY2bdzzW+z4rZNHFXBFJxzMXL7Z4kabSf79Fwf+339x4P/x65P+32gzyZ+5fLMFXi/AgP39RnP2pJ4JML6EoB7D+8C4Cfb3G82JMgAZJKTEMrx4f4mhorQbpxq8BiyYs5hh2eqFDB3dpcQboFF1ZuLPeSUFL95fYmBgYGAwslTAZwd2F2hUnZkouH5RwYv3lxgsTz8swGsCPnCjzSQfmcYGADbgmyQXZAtdAAAAAElFTkSuQmCC"
    },
    "ORBIT_DISCS": {
        "name": "Sawblade Orbit",
        "description": "Makes four sawblades rotate around a projectile",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAvklEQVQ4jaVSsQ0DIQy0f4Xoy9dLrzRpMkN2YYGMkAG+iZRBUmYHSyloaNK5RJnBaR7JIIN45RrA9h2HMUADPrC08gAAQy3xfL1Fr7sEfGBZ5hHu6w2WeexyYjrwgUU7iDE+miQiupROjJpMpNoDAIDzaUIj/Om6vQUiuloOJhHJLMsGQ+ObCaQiRER97sGgiIcUTEK4weCxqabfVkPZq/IXjiVh9xDpYbGGqRs+sPjA4pyTtN8t8pcD7aSV/wEMq3rQdz/GcQAAAABJRU5ErkJggg=="
    },
    "ORBIT_FIREBALLS": {
        "name": "Fireball Orbit",
        "description": "Makes four fireballs rotate around a projectile",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAoElEQVQ4jbWSPQ7DIAyFY9S5A0P3dmHiBDkVnXKLLr1chJTsVSYu8LLElWO5+VPzTfiBnw24qs4kR4+SAg4llxSAoQEA5Oj3m+ToAQAYmm8XJYX3WtVad6Gra5PLkuH11ZIhd5uqL1FSePLaCf0GYNYuJgyPfmbAh4iIZLwFJxLvLLIRTRh5H9NN3u0X+q2c2n/I4NAk8j+fMom7sSbx74x4X2qtEe13qgAAAABJRU5ErkJggg=="
    },
    "ORBIT_NUKES": {
        "name": "Nuke Orbit",
        "description": "Makes fourΓÇª nukes(?!) rotate around a projectile",
        "meta": {
            "action_type": 2,
            "action_max_uses": 3,
            "speed_multiplier": 1,
            "action_mana_drain": 250
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABV0lEQVQ4jZWTP0vDUBTFf6+IQ0fDE4p1sAodu3UR4mIFV3HJ4ifI0u6iTvkC+Q4B/wyuQpF27ObkVM2gFlK7tODQ5To0KclL6p8DgXBzznnn3XujMBAEAePxGCYPYB1xunUlb6MJAE0XZfJLZsFxHLTW8hxti9ZaenOfoRUwUsc5McBaQQIBOD+8pje32Z04ySeBPySIokiFYUjnpkUYhnRnHt2ZR9NFTdt1vyjFEtN2fV9ESD8fd5YMfGTgIzEnY5K7glK5lGbhZeXpP8bLcjvJe7oHmyIiaaLEKPB4zRgkJBXnXyEqRCkl3EmKiZGKUaCLCt3Sd1sFs1fmFGoAyciqFYvb9wuVrLXWGgaXGUEu3rRd9x/3au7XxtmyD/1+H9u2cRzn1xmTTgAwtAIO1l1688X+mCa5RYLFX+d53sJkFnL/1KLRCCmXy/9LUK1YVE4+MxxzKN9N4o0VFQY56wAAAABJRU5ErkJggg=="
    },
    "ORBIT_LASERS": {
        "name": "Plasma Beam Orbit",
        "description": "Makes four plasma beams rotate around a projectile",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA+UlEQVQ4jaVTO47CMBB9g7anYgvkG1AlnGELzhDJ0p4kCpI5SCSfwQgac4M0oaNhoy2SBk4wFMR8rDiJdl9lz8x746fxAAEIqQ23EFKbUN0klHDYX/vzgwJ/gpDaKMsPKHu3oSynY8hNnJXsI85KFlI3vsibBWU5qvJkVp+K7ebwjG8OQH0qtlWezACcOzsry5FvI85K19l4td/u/PES/2RmJiICgCpPVj/MDABEi5XX7/fNAj8K72R3H4PJC3HugkREQmqzXB+xXB/R8ZHqTjXnLTTGtibqJLfJdGiMg55akeALRsMt0+7yj2XaX4GvaX+jXoEhMgDcAJowzVjuAC3cAAAAAElFTkSuQmCC"
    },
    "ORBIT_LARPA": {
        "name": "Orbit Larpa",
        "description": "Makes four copies of a projectile rotate around it",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAhUlEQVQ4jWNgoCX4/////4+7/v8n24CPu/7///8fYsCPq/+nUOyiT2sJGPJp7X8dUg1hIsMhV8i2HUltBjYXiMMCDAb+/0cEIhq4jWIATBEjIyMjMp8YwISkURUmCDOIEQqw6HuJ1TRkv+EC6GGFHgtEByQ+GyhPcbBki5yUyQIUZyZiAAB6jlRfkcC1pgAAAABJRU5ErkJggg=="
    },
    "CHAIN_SHOT": {
        "name": "Chain Spell",
        "description": "Causes a projectile to cast a copy of itself upon expiring, up to 5 times",
        "meta": {
            "action_type": 2,
            "damage_projectile_add": -0.2,
            "speed_multiplier": 1,
            "spread_degrees": 10,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAATklEQVQ4jWNgGLTgPxQQUsdED8eQBv7fKIQ7+3+yIkEvoGqeGoSiAZcBWMMHm2J0A3EagM+puAwhGiCHCTY+SQZQ7BqyAKEUOQhTIqkAALLnQp0xifObAAAAAElFTkSuQmCC"
    },
    "ARC_ELECTRIC": {
        "name": "Electric Arc",
        "description": "Creates arcs of lightning between projectiles (requires 2 projectile spells)",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 15
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAd0lEQVR4nLVSOwqAMAx9iuAtu3ui7q238ACu9TiFlDgEilQbjOAbMiTvRynwAczMzM0yJnokDyIAsB4FwLaXsMyGtJio592F81m5TvcEAMGUYKsk7EajlazUVzlXJ+dzTCTTYCEk57P+dN2eAEaDzoT6335LqDgBC+lFNFlDYioAAAAASUVORK5CYII="
    },
    "ARC_FIRE": {
        "name": "Fire Arc",
        "description": "Creates arcs of fire between projectiles (requires 2 projectile spells)",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 15
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAoklEQVR4nGNgIAP8////////DAwM5wzkCSpmgrPOGcjLhUkS1MbEwMDAyMh43lBBc7kPRMjowkOiHPamzeL79ew3bRZv2izgIvic9GjVcwYGBu5AY7iISNUJwvacM5D/fj0b4g1iwgChB+42orSdM5CHeAlTiglTiAEpoCCWYPU9FgCxBM1J2G2AAEi4aS73Ic0eiH+IDTG4Hnh6IxaQrAEOAOusT6on2KrLAAAAAElFTkSuQmCC"
    },
    "ARC_GUNPOWDER": {
        "name": "Gunpowder Arc",
        "description": "Creates arcs of gunpowder between projectiles (requires 2 projectile spells)",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 15
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAxUlEQVR4nGNgIAP8////////EPaHShP8ipngrO/n4z9UmrCHaePXxsTAwMDIyPixypRFwQKuWqD9DFwFTs2/30///X46RPr7+XgC3oC45/v5+N/vp0NUQ0Swq4ZLIGtDU41FM7I2ZOd9qDRBVo0IJbhHf666+ufBCQYGBvYwbYjzIIGBz20QNsQ/8AAgEEUQpfAwgKtmwqOH03DhnwcnWBQs4BGKXQPEME7DhXD/sIdp/1x1FZ970Bz9odIEOb0RBUjWAAcAUy2GP5JQHTMAAAAASUVORK5CYII="
    },
    "ARC_POISON": {
        "name": "Poison Arc",
        "description": "Creates arcs of poison between projectiles (requires 2 projectile spells)",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 15
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAx0lEQVR4nGNgIAP8////////RCpmgrOqteuJ1cDIyFij0yAjI0OMHmYIJfVNgoWJTUZGxoDD6PDrg4Q1XP191U7M4dOnTwT1MMNZUt8kpj+dyvOCS0/HgOcF19XfVwlogKgIUAs5d+mUg40LLnuYMIVWfl355MkTXGHAjMaHmHr49UEDDiOs/kG3IZw7HM4+cGSPjIwMsggWG+B+lfomsfLrSgMOI7QwQNeAphMSP3o6BgTjB+o2iL+rtetJSm8M4dzhpGlABgANZ13S/DxJlgAAAABJRU5ErkJggg=="
    },
    "CRUMBLING_EARTH_PROJECTILE": {
        "name": "Earthquake shot",
        "description": "Makes a projectile crumble the earth it hits",
        "meta": {
            "action_type": 2,
            "action_max_uses": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 45
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAqUlEQVQ4jaWSsRHDIAxFgRVcuM0OZAjfUXgOT8QcKbhjCLwDrQvPoFTKIVlSzskvhd7XF+Dcn/LaQU4RpPpWdsJcDBCsrYvGy/NBTIhBThE0EOHaOjEJv8C19U/SoBKCXsfpt7J7MYE1mWu8A9NA2tk5+kKqAUISPN4BUU4R1nmCdZ5A+wc8gWqCTTlF08ycYCUSnxGL/LtKuvTw6GN9rH3d32rUhtwSh9/LvnowaU23uwAAAABJRU5ErkJggg=="
    },
    "X_RAY": {
        "name": "All-seeing eye",
        "description": "See into the unexplored. But not everywhere...",
        "meta": {
            "action_type": 6,
            "action_max_uses": 10,
            "speed_multiplier": 1,
            "action_mana_drain": 100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAA8klEQVR4nI1RsQ3CMBC8R9DTEBZAYgEERNAgZQQGYAN6Qgk0FIgNGIANsJSGKBHKBmGBkCZ9EKYwMs4bAScX/++7f9+bYCHwIx1P1i67rdvsvViZKdOQzR7B63lDAImIQ4i5t7TnvNjbwS7wozKVUj6kfJSp1MXKBJWPZ0MA9Y5kje5XAnA+xMoSmRMSEQNQ71HQFf2qiukQonGTEO/K5XYqHerh3aIiyIq87bRCU+FQVuQ/tsQ8sC3V2DXrx9IPCPzI7fb1MRf6eQKAdrPFAhOkG6tgcZybvKzIN9O9ipWT18cxng2lnKxd+odtasi29R1P1Vd7Cm4Tm4YAAAAASUVORK5CYII="
    },
    "UNSTABLE_GUNPOWDER": {
        "name": "Firecrackers",
        "description": "Makes a projectile release firecrackers when it disappears",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 15
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABGklEQVQ4jY2SoU4DQRCGv70geiF9AJIKBAIPooJ6EsS9QEUFD4DsPcEhcdiKSsw5BO4qTvAAiBMIkktFFWkWQTKIZcv2drfcbzazs//sP/+MIoDP4liG+VY93lzI8+t76AkA5XqjVCyp20IAFrdPHCqSRDNNHU258BToKpN0UiqA/1oIKrBki+vLUwCWd1/9FMCfiTY+pCTowTDfKl1lAqal2Xy0U9JLgYWuMuFsbIKmZnH/4U0kARARCZ078i9m85HnxRGAUmYfuidNbYrs/T7wFcSQTkplydOrt+CbBIzr7qUb25+nD4MuF+iYqNtC0pO81/i8FnRbiLu+VkVsfEEF3QWyRb9XL9EW9kx0yQCWvFydB8nleqN+APVNiiQeoS9xAAAAAElFTkSuQmCC"
    },
    "ACID_TRAIL": {
        "name": "Acid trail",
        "description": "Gives a projectile a trail of acid",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 15
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAqUlEQVQ4je2QsQ3CMBBFv7MCsuis7OAMEcmSmSNpPAuV56BAyhBmB4uOghk+1UVOopAgQcdvzjr7vTsZ+Oe3CdHQ95q+1wzRUHokuQqU1feapUhg6W9OLyW7Hs/PH+dr8Pwv3kXtHRCdZXe9qejsRDoRhGh47u4LaXSWQ8pomxpDypO7qjQKXK4uMIAFDADVkDJEIlVEJbwWBQCn44GyXtvU4+UWfHk81Quee2YPk0QIyQAAAABJRU5ErkJggg=="
    },
    "POISON_TRAIL": {
        "name": "Poison trail",
        "description": "Gives a projectile a trail of poison",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAVUlEQVR4nGNgGIGAEavorYwNJ2/dZGBgMFdTV5sRcCtjg+p0f0ZGRnRFcHKxUyeEvdip81bGhv///0NEsANkPfhUEFCERxtpqpF9Qm1Apqmk+ZsmAACa+Cn54+lYFQAAAABJRU5ErkJggg=="
    },
    "OIL_TRAIL": {
        "name": "Oil trail",
        "description": "Gives a projectile a trail of oil",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAnklEQVQ4je2RvQ2CUBSFv2v1wgBOwBja6gbQ8DMENRW17kB8S7gLEziAoTtWJKA8xFjY+CW3uj/n5B748zUGIGm2maZ7ORcBEMdb6vpigLyvyPOzAWyeFwDK8igA5yLa9mp9f6frbjRNLu8rsuw0VZL0UkVxkCSSZKe5/oTx0LD4rmYdhNRWHRgrD4eW3KxKYQkzI5jCx4QSWP2Dn/IAjanZ2GXnOGYAAAAASUVORK5CYII="
    },
    "WATER_TRAIL": {
        "name": "Water trail",
        "description": "Gives a projectile a trail of water",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAIxJREFUeNpi/P//PwMlgImBQjAMDGDBJ8noP/k/A6cQhCNvxfC/U5ERyPovPpmB4WUuAyOGC8AaQHTEUkjUADX/XxHNyPD9HQPDw2MMjOX3YZoRABSN2DBD+JL/YNpv0n9casDy6IpgGonFqBwCtuE1ANlmmEHEuIaRqkkZFgskAXwxQAxmHPDcCBBgALhnBWYprvovAAAAAElFTkSuQmCC"
    },
    "GUNPOWDER_TRAIL": {
        "name": "Gunpowder trail",
        "description": "Gives a projectile a trail of gunpowder",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAARZJREFUeNrcki1uAmEQhmdoJYoE04RUIorkBBUYZJtUcIZeAcFNEBWQNMHUVPQEOKiobEgwTaqwZDrPsLOEbKhBkDBi98u3M+/frJqZnFI1ObEuAOCax8fwQd4/t9K7u5Lf7620+y35elvxKRJu3O7u/a3Nzo39LNdyP3rVUgHD3brQZDS/TFbxphxMExRAgB2kVKCscTF9DkaaQKdgciB1VZZsRy34kMEEyHwj5mp8YK2FKv0vgwMFLtsYcJCwlDbSgiuKe+pxPNtvAdkwJxuyAcFGEV70DJ5aJejBGmnyoWAuQsU7jHHHd4IDhHMlg5C8A2FtypnGlAtrhuzZVBXAVkiOlWEHjwwylBmkgsoaz/or/wkwAMvKqa/NedYWAAAAAElFTkSuQmCC"
    },
    "FIRE_TRAIL": {
        "name": "Fire trail",
        "description": "Gives a projectile a trail of fiery particles",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAPElEQVR4nGNgGH7g+27nwWD214kuaNq+368mXzNRqiGM7/erIYyvE13+//+PUxvcPXBt////p2XoDX4AAFxkJY4qlL0mAAAAAElFTkSuQmCC"
    },
    "BURN_TRAIL": {
        "name": "Burning trail",
        "description": "Gives a projectile a tail of fire",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAhklEQVR4nGNgoAr4Gi+PS4qJoAo8UgjwY1kGARuwGonVbCasSpEtQdOGxQZmd088bsPnaaYOLD5hgVDcCx8iG/nfkVCwIjsGrppx/0OIILL9WPyABv7u3A63H13D353b4WyI8QScBDEJohTuMGTjsTgJbglEG7KdUHFMS7/Gy0N8j+Z6MgEAHEQ4ceIXKn8AAAAASUVORK5CYII="
    },
    "TORCH": {
        "name": "Torch",
        "description": "Lights your wand right up!",
        "meta": {
            "action_type": 7,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAABnRSTlMAAAAAAABupgeRAAAAmUlEQVR4nGNgoAq4bsZHHYMI2IDVHhasGuT8Fa8z3McqxYTHHjl/RaJsuG7GxxRkIwdja576RNiGf+uOcFVdYAqywbQEiwY5f0WmIBsGBgYOjSmYslg0MAXZYFWKU8O/dUdwqcaugYGB4ceNHAjJXX0RTYoRjX94ug8DA4OJgzwDAwOn5lQ8VjEwMDDMbwgqiLIkoIhM1cQDADDzI0HuVTeIAAAAAElFTkSuQmCC"
    },
    "TORCH_ELECTRIC": {
        "name": "Electric Torch",
        "description": "Gives your wand a bright but very dangerous light!",
        "meta": {
            "action_type": 7,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAjElEQVQ4jbVQsRGAIAzMWuxgZekUDkDhFpYWjOIwdKzwFlzuYkiAQr97+M/nQ/QHYgEkDynD0347VIs0gPrf1bHISgCAWIBhJR6it7PeG4SUwdBcJps1YqkrykTL7CZrLodMdfeqSEwPIKrH5HTJrY0b3OeCfQvm8bje0CxN8r9rvo71ZWZMHdAz/4oHaw3X2wtclDAAAAAASUVORK5CYII="
    },
    "ENERGY_SHIELD": {
        "name": "Energy shield",
        "description": "Deflects incoming projectiles",
        "meta": {
            "action_type": 7,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAq0lEQVR4nKVTwQ3DIAy8WB4hG1RIGYUHQzBah8iDUSJF3SAjoLSPlIgYQ1NxP/Dd2cABdGKoFcIrvvO1fbDK5ZpwXqO6L41YkpJw2fYA4PkteSDaxMlNBikWQgk/jWSd4XMSUkg1sVojpXsTy7aHeY3nncgJWt1VjnaEvyAN/A3NhUPA8bbOMKaR7C/1nVdoTVHULqlqBSlNl3cvDJIJUEbZmSO0Msrdn6kbH3CWWxALCFpfAAAAAElFTkSuQmCC"
    },
    "ENERGY_SHIELD_SECTOR": {
        "name": "Energy shield sector",
        "description": "Deflects incoming projectiles",
        "meta": {
            "action_type": 7,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAmklEQVR4nGNgoBAwogu4LPkWriHCtMJHlYWBgYGBwUOZBUMNXgOQDElgYGDwIGQQTtNdlnwLZ2BgYNAQYVrBwMDA4KPKgtUQvM6DGcLAwJCgIcLkgc0QJnwGIIEFuCTwGrAnhmsljH3jzb8dW27/Ydhx989/clyA0xWkGIAVkGJAAskGIMUCA6WxgNV2BgZaJSSKkjKpmYliAAD+Xy0WhOiveQAAAABJRU5ErkJggg=="
    },
    "ENERGY_SHIELD_SHOT": {
        "name": "Projectile energy shield",
        "description": "Gives a projectile a shield that deflects other projectiles",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 0.4,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA50lEQVQ4y8WTMWrDQBBF34jFvoBuYAy6gg9gosKVG7cmfbrcJF0g4CZFehdKkXsIhG6g1hjDokmjEZKiJUpUeJoPw/75M39mRVWZExEzwwGICABZ6QE4F773KK9qnjcLTVdOLGedO0tkpW+JeVUPhR7PhScrfa9IO4KR86oeI78Cl7yqMyvyVw++GjwFTTT1QHx0vPjRhQN4eViGyAqwfb8KcGi6SMdG0ABOW+PT502aEczhIR4aPI56sFs7kjjop5FJ4ijdrR3dVU7Zwj6kDiCqioj8ekhJHL111e0S2wL/PWW5+2/8BtsqeVEP0rTUAAAAAElFTkSuQmCC"
    },
    "TINY_GHOST": {
        "name": "Summon Tiny Ghost",
        "description": "Summons a tiny ethereal being to your help. It may cast stronger spells depending on how much damage you have suffered.",
        "meta": {
            "action_type": 7,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AMaCjAdiNLh4wAAABR0RVh0RmlsZSBOYW1lAHRvcmNoIGNvcHnSYMZFAAA542lUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgNS42LWMxNDggNzkuMTY0MDM2LCAyMDE5LzA4LzEzLTAxOjA2OjU3ICAgICAgICAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIKICAgICAgICAgICAgeG1sbnM6cGhvdG9zaG9wPSJodHRwOi8vbnMuYWRvYmUuY29tL3Bob3Rvc2hvcC8xLjAvIgogICAgICAgICAgICB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIKICAgICAgICAgICAgeG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIj4KICAgICAgICAgPHhtcDpDcmVhdG9yVG9vbD5BZG9iZSBQaG90b3Nob3AgMjEuMCAoV2luZG93cyk8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgICAgPHhtcDpDcmVhdGVEYXRlPjIwMTktMTItMDhUMTY6MzY6NTgrMDI6MDA8L3htcDpDcmVhdGVEYXRlPgogICAgICAgICA8eG1wOk1vZGlmeURhdGU+MjAyMC0wMy0yNlQxMjo0ODoyOCswMjowMDwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXA6TWV0YWRhdGFEYXRlPjIwMjAtMDMtMjZUMTI6NDg6MjgrMDI6MDA8L3htcDpNZXRhZGF0YURhdGU+CiAgICAgICAgIDxkYzpmb3JtYXQ+aW1hZ2UvcG5nPC9kYzpmb3JtYXQ+CiAgICAgICAgIDxwaG90b3Nob3A6Q29sb3JNb2RlPjM8L3Bob3Rvc2hvcDpDb2xvck1vZGU+CiAgICAgICAgIDx4bXBNTTpJbnN0YW5jZUlEPnhtcC5paWQ6NTBjN2ZjYmYtMWRjNy04YjRjLTkzYjEtMzg3MGVjNDQ5YTQ1PC94bXBNTTpJbnN0YW5jZUlEPgogICAgICAgICA8eG1wTU06RG9jdW1lbnRJRD5hZG9iZTpkb2NpZDpwaG90b3Nob3A6N2JkZDRjOGItNjNiZC02OTQwLWE3MzUtMjhmNDgyZWI1NWIxPC94bXBNTTpEb2N1bWVudElEPgogICAgICAgICA8eG1wTU06T3JpZ2luYWxEb2N1bWVudElEPnhtcC5kaWQ6MDRiMzU5OGQtNmZlMC0zNzQzLTg3YzMtNjQ3ZmU0Nzk5NjJiPC94bXBNTTpPcmlnaW5hbERvY3VtZW50SUQ+CiAgICAgICAgIDx4bXBNTTpIaXN0b3J5PgogICAgICAgICAgICA8cmRmOlNlcT4KICAgICAgICAgICAgICAgPHJkZjpsaSByZGY6cGFyc2VUeXBlPSJSZXNvdXJjZSI+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDphY3Rpb24+Y3JlYXRlZDwvc3RFdnQ6YWN0aW9uPgogICAgICAgICAgICAgICAgICA8c3RFdnQ6aW5zdGFuY2VJRD54bXAuaWlkOjA0YjM1OThkLTZmZTAtMzc0My04N2MzLTY0N2ZlNDc5OTYyYjwvc3RFdnQ6aW5zdGFuY2VJRD4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OndoZW4+MjAxOS0xMi0wOFQxNjozNjo1OCswMjowMDwvc3RFdnQ6d2hlbj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OnNvZnR3YXJlQWdlbnQ+QWRvYmUgUGhvdG9zaG9wIDIxLjAgKFdpbmRvd3MpPC9zdEV2dDpzb2Z0d2FyZUFnZW50PgogICAgICAgICAgICAgICA8L3JkZjpsaT4KICAgICAgICAgICAgICAgPHJkZjpsaSByZGY6cGFyc2VUeXBlPSJSZXNvdXJjZSI+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDphY3Rpb24+c2F2ZWQ8L3N0RXZ0OmFjdGlvbj4KICAgICAgICAgICAgICAgICAgPHN0RXZ0Omluc3RhbmNlSUQ+eG1wLmlpZDo1MGM3ZmNiZi0xZGM3LThiNGMtOTNiMS0zODcwZWM0NDlhNDU8L3N0RXZ0Omluc3RhbmNlSUQ+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDp3aGVuPjIwMjAtMDMtMjZUMTI6NDg6MjgrMDI6MDA8L3N0RXZ0OndoZW4+CiAgICAgICAgICAgICAgICAgIDxzdEV2dDpzb2Z0d2FyZUFnZW50PkFkb2JlIFBob3Rvc2hvcCAyMS4wIChXaW5kb3dzKTwvc3RFdnQ6c29mdHdhcmVBZ2VudD4KICAgICAgICAgICAgICAgICAgPHN0RXZ0OmNoYW5nZWQ+Lzwvc3RFdnQ6Y2hhbmdlZD4KICAgICAgICAgICAgICAgPC9yZGY6bGk+CiAgICAgICAgICAgIDwvcmRmOlNlcT4KICAgICAgICAgPC94bXBNTTpIaXN0b3J5PgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICAgICA8dGlmZjpYUmVzb2x1dGlvbj43MjAwMDAvMTAwMDA8L3RpZmY6WFJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOllSZXNvbHV0aW9uPjcyMDAwMC8xMDAwMDwvdGlmZjpZUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6UmVzb2x1dGlvblVuaXQ+MjwvdGlmZjpSZXNvbHV0aW9uVW5pdD4KICAgICAgICAgPGV4aWY6Q29sb3JTcGFjZT42NTUzNTwvZXhpZjpDb2xvclNwYWNlPgogICAgICAgICA8ZXhpZjpQaXhlbFhEaW1lbnNpb24+MTY8L2V4aWY6UGl4ZWxYRGltZW5zaW9uPgogICAgICAgICA8ZXhpZjpQaXhlbFlEaW1lbnNpb24+MTY8L2V4aWY6UGl4ZWxZRGltZW5zaW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAKPD94cGFja2V0IGVuZD0idyI/PmKLDnIAAAAYdEVYdFNvZnR3YXJlAEFkb2JlIFBob3Rvc2hvcDTLjmcAAAAPdEVYdFdyaXRlcgBTdXBlclBOR8XEr90AAACfSURBVDjLxVM7DoUgEJw1HsXkVa/iEjbYPw746LHhElZWJt5lbQwBwoqGwq32O9mZBWJmtFiHRmsG6KXCf15ibmS0ur9BNgwAXMiVAaRGqdZlDcH3zgIA9m1N4jsUCADG6Zck87gIUBJq+HyfXcFoFajEa0sbiGf0ziZDeRz4xk/5VJkAhOS+rQkNoxXVRGRJA+8s1a5AF3pRlcIrn+kAtao/KQ1kvm0AAAAASUVORK5CYII="
    },
    "OCARINA_A": {
        "name": "Ocarina - note A",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAVElEQVQ4jc1SQQ7AMAgiS///DHwmPfXSuEXqoeOGERQj8FuQVMtAUsngaU1xDaxYWYRqrLR58b1uRTje4G0bABiVCRHxyVOxdfG9+egLSWrBFl/FBL5RTWPJJt8PAAAAAElFTkSuQmCC"
    },
    "OCARINA_B": {
        "name": "Ocarina - note B",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAX0lEQVQ4jc1SQQ7AMAjSZa/0gfhMeurScZltl2ycNAEFo9lvAYBbA0iWBhxbW8zsrJA0TkT4o2iMoHHGvuRARe7+noOlI2bmVd+s6KZutXREFU89kpKXvhAAO6bFn6IB8CtIkD5KulAAAAAASUVORK5CYII="
    },
    "OCARINA_C": {
        "name": "Ocarina - note C",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAV0lEQVQ4jc2SQQoAIQwDZf//jvSZsyfBLa5EBTG3ghmb0FKulSS2AIAFeLZ+mQFIAsCO1kbIJgvSAkZ9nOkgIj7zbwSSsmlYYs/gbNd9vHSFdUX3Au/RCzRzWRaPcHI1AAAAAElFTkSuQmCC"
    },
    "OCARINA_D": {
        "name": "Ocarina - note D",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAW0lEQVQ4jc2SUQrAMAhD6+j9jxGPmX21ZELBWhjNl6I+IdratQLAIwDJFOA52pIFAOBQmqzNGqe9WQFi/o8HKgA0Mxt51+LKJHefNR3+KA5vPVJsLn1h6dZX6AWCaU1e9/UBywAAAABJRU5ErkJggg=="
    },
    "OCARINA_E": {
        "name": "Ocarina - note E",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAWUlEQVQ4jc2SQQ6AQAgDwfj/Z5Rn1mvdZLW7HLRHAgMFIn4rAGwBSFqAo9XFBQCgyrKnFp7s2BZmnU8XkJnZmmCmG0AXpfGqitcljkVLjzQmb32h3nq5+FNdE45cQS1lo1YAAAAASUVORK5CYII="
    },
    "OCARINA_F": {
        "name": "Ocarina - note F",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAVklEQVQ4jc2SQQ7AIAgExfT/z1ieuV6pCXWVg90zDEygtd8GAEsAkhKgl6aoAACMkfSiwpeOrJBNflSAmVlpgyyvDTJXd1+T5uatR5qLj74w3nq7+WoGsA5FZv/E22gAAAAASUVORK5CYII="
    },
    "OCARINA_GSHARP": {
        "name": "Ocarina - note G#",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAaUlEQVQ4jc1SQQrAMAjTsS/1bf1RPtNHZadAV1pW52HLJSgmUdDstwDAlAHJLYMjlbJr0FqjNhr5fBKTpLu7mVmtlTLs66Ww51U/dEJqgxX7TCTodiWWUm7z00Qh9Ejj8KsvBEAhLP4UFwmpdY0srb5lAAAAAElFTkSuQmCC"
    },
    "OCARINA_A2": {
        "name": "Ocarina - note A2",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAYUlEQVQ4jc2SMQ7AMAgDTdX/PwOeSSdLEUlbpxlSLwzgQxYAv5W75xIgMyXAsbRFBdQ4UjxGqJXm14h3gNqXIozMZmYygMPVDADn0yYAiIiucq4FdeapR/p0phGEmjZv1QXyAmFAQ1akeQAAAABJRU5ErkJggg=="
    },
    "KANTELE_A": {
        "name": "Kantele - note A",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAZUlEQVQ4jWNgGLRgZ2vcf2LUMeGSMHcyI8oinAYQC0gyAJu3SDKAWG8xMDAwMHw4PuU/Nj66OEkuQNdMtGJcrmFgYGBgIcaGk/tO4eVj1UxsQsKqmBTNjMiaYNEkYJnDiFPHoAMA0m00se1Ro+oAAAAASUVORK5CYII="
    },
    "KANTELE_D": {
        "name": "Kantele - note D",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAbUlEQVQ4jWNgGLRgZ2vcf2LUMeGSMHcyI8oinAYQC1iIUbSzNe4/zEUCljmMRJn84fiU/9jY6GFDshfcqxcxIhtIcRiQbMDO1rj/yOGAEojITkMGJ/edgsvhDER0zcQmJKyKSdEMdw5ZcT0oAAAotTFDt2+DeQAAAABJRU5ErkJggg=="
    },
    "KANTELE_DIS": {
        "name": "Kantele - note D#",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAlElEQVQ4jWNgGLRgZ2vcf2LUMeGSMHcyI8oinAYQCwgasH9C3v8Px6fAvQNjw2gWQgYYmqsxCFjmMH44PuX/hlW7GBgYEOGzoNDvP0leSOjfxMjAwMDgXr2IEcYnyYAFhX7/qe4CRmQFyIHFwMDAIGCZw7h/Qt5/WDjgNR1dM7EJCatiUjTDnbWzNe4/LPURdO6gAgAb/kNWisW2tAAAAABJRU5ErkJggg=="
    },
    "KANTELE_E": {
        "name": "Kantele - note E",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAdklEQVQ4jWNgGLRgZ2vcf2LUMeGSMHcyI8oinAYQC1iIUbSzNe4/sotO7jvF4F69iJFoA8ydzBgELHMYsckR7YUPx6f8xxawRLmAgYGBchfgAigu+HB8Cta4P7nvFIocciDi1ExsQsKqmBTNcGcgxzWuABucAAAA1S4aA9r1PQAAAABJRU5ErkJggg=="
    },
    "KANTELE_G": {
        "name": "Kantele - note G",
        "description": "Music for your ears!",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 1
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAeElEQVQ4jWNgGLRgZ2vcf2LUMeGSMHcyI8oinAYQC4g2YGdr3P8Px6f8J9ZrDB+OT4ErRNdElCHIBiCz0QELUc5BshkWuAKWOYwMDESGwcl9pxgYGBgY3KsXMcI0wgAKB92pyIqx2Y4C0DUTHdrYFJOimXgnDloAAJ/GOnQtbLATAAAAAElFTkSuQmCC"
    },
    "RANDOM_SPELL": {
        "name": "Random spell",
        "description": "Casts a spell, any spell, at random!",
        "meta": {
            "action_type": 5,
            "damage_projectile_add": -0.4,
            "fire_rate_wait": 35,
            "speed_multiplier": 1,
            "action_mana_drain": 5
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA7klEQVQ4jZ2SMW7CQBRER1GKXCInyAHwLeKaNlVOMXZDgehpIpdGSvUpUpDCSkExXCBn4ApUPw1r1maXAE9yYVnz/l/PAhHsZLgXdjJ39yCZU8b2+OhrIBab9KA47COCRGzM3T0r+UmET5JZL7kqvBRtKVpKEniMX57wC2ACAPjeVev3oi4BYK9nqyZvrwDwgsOFP3lcbyP2U6iPwQaf0bd/Cc0E1GXCLc/7vyns7j6WcHESqMmv/TCti3JV7dbTuiivPtqoiSwtZeJwOjWzVJ09oYE2uhMpSTbs7r4RTdxGgszNu7QBAIhbuyl8D39iLR8oqjnmUQAAAABJRU5ErkJggg=="
    },
    "RANDOM_PROJECTILE": {
        "name": "Random projectile spell",
        "description": "Casts one random projectile spell",
        "meta": {
            "action_type": 0,
            "damage_critical_chance": 5,
            "fire_rate_wait": 3,
            "speed_multiplier": 1,
            "spread_degrees": -2,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABB0lEQVQ4jbWSv2oCQRDGP67yIVKkEAzY2MUHSGcRnyGpfAYPBpsUaSzTiOXVC5LimiMQwUkTUknAA5Mqz2D1pXBH1vXM5Q/5YGHY3e83s7MDeEmhDr+VFOpI0iCSqZNMneh9JVRlerhv5lvZwkyiNy42k2Ql5MGbcxV359cxiMVJeNDAchcPuqO+xW1s9hJ1R1d9HFOusqOLTrblFvLzBltj/2aeykfd3QQAMolm4BnnAIB3PNVmy3znY8gBtA5SBQ2/a7bukSRXHPJbUJW5Gz+2aMZQISSJjfaNi4vry+ZJE2enHZRIUSIFAJRI8fr2Up0195MXzgIAWCUrDjlb975+Qmz+d30CnRvUf7GWm1kAAAAASUVORK5CYII="
    },
    "RANDOM_MODIFIER": {
        "name": "Random modifier spell",
        "description": "Casts one random modifier spell",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABMklEQVQ4jaWSMWrDQBBFR65yl4Bbu0xK40L2FYILp/UBdnATllwgTTCBBRVpvC5McKOkSLFVQAhSyLgyW+QEJtVPIY2QZQniZEAwOzP/7fBXREVw7Cz9NTh2FgAEwpGzHDnLbt0Idbw4rYv4nnOYBLs7WxcDQCPkrRBvHNuH4muDSN6pNi7os8xv+/OR5Jf0fXRRf34zorbYOC7p7B7zdWM+32Ax9l9irTWMMZD6bDpBfbZDRBRx7R/4oB4R0dXhsPLek9Ya4XCA3d6XI0mS5LCocL4OqZ5n0wnC4QCZUsiUgjEGmVJoHK5CHS+siEVUFwdtPjh+t89fT+Fu72m1fgnEi952Sz5N6Xq5DEoPqiHPWBVLr9vtkk/TtjtzMQDI2lKXtV/H45NXaIS09X4FODd+AN8u9h1fPgoUAAAAAElFTkSuQmCC"
    },
    "RANDOM_STATIC_PROJECTILE": {
        "name": "Random static projectile spell",
        "description": "Casts one random static projectile spell",
        "meta": {
            "action_type": 1,
            "fire_rate_wait": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABIUlEQVQ4ja1Sq07EQBQ9SxDwDV1bDLpQRbKCYBCrV00wBMUnXGoQ+wMYskGQ6kEgNk0azCZ3DRKP4hMa1EHszOy0tILHTW5y7sycM/cFOJNaLX5rUqslSS8ipVop1Yo+94qqLL6fe/JcNmLeRG9tl0ySvSIvjrxUsXfOh0Q83okv9vAW8FVeTD0+xGfro7y4mGLIlipBXfR+k24tP2+wb+zfyAv5AABTNYw9frsLAKWonRX5tq5XHGMC4B1rUzVcmZPWB6Zq+HC6P4Ink2Qp7UUqRa2pGqZJxjTJKJePAadJ1s6kj0ySsUDs/g5wY2yl7+L1zeoJAGbn19tzhw/GR8MNjMfYLYFkKKXbzED2mzgk0iWP+kTOoi30InEcJvAf9gWkEv14sDSoOwAAAABJRU5ErkJggg=="
    },
    "DRAW_RANDOM": {
        "name": "Copy random spell",
        "description": "Casts a random spell among the spells in your wand",
        "meta": {
            "action_type": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA2UlEQVQ4jaWSIQ7CMBSG/ywI7rIDsGPsDDjEzvAyg0BiMGSyugYxsyAQD4Na8CjOQCYeYu3SdW3C2J80eW3zf335XwEjaljjX1HDWkTEQkixJsWa+BKEMlXTc2s+UA+zIt5r3ywiEoRcjblm0iezYhBbJ+7FGs+h3mVlbusUn9FDWbnNEVPNNNCJz327Dc0P2Aa7zFzR27/r2kLcfQIAirw/8MAGAPDC3Tev0uOYqEzyPsTfd20xjHbScsgcmnUUEBLTbRLeTwB3jLMBtfl5MciiDtwQ/VEu0hdInd3ErrIJeAAAAABJRU5ErkJggg=="
    },
    "DRAW_RANDOM_X3": {
        "name": "Copy random spell thrice",
        "description": "Casts a random spell among the spells in your wand three times!",
        "meta": {
            "action_type": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA50lEQVQ4jaWSoQ7CMBCGf6Z4Fx4AHmOPsOCQe4LLDAJRiyGT0zWIhWRBIA6DJEGieAbUIdZrRrsljP1Jk8u1/9c/1wJO1LDFv6KGrYiIQqhiSxVb4mMvlKmM+2reUQtTEW9taBYR6YWcnblmsnu3hiBaJ92NOe6+3qyKVOsF3l8XrYp1iiHVTJ5OfGjjNjR+wDrYaeaSXt2+n6wxor0EACoK/sANSwDAE1dvNkZmTshzeEjlJh9CImiYpJMiOqxQPSQiMmgeEtPFhuafIN1nVHMI0Vn2mvUnhnuPLDuNThBFNyZ6xsn6AM9G8IX8lROSAAAAAElFTkSuQmCC"
    },
    "DRAW_3_RANDOM": {
        "name": "Copy three random spells",
        "description": "Casts three random spells among the spells in your wand",
        "meta": {
            "action_type": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 40
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA5UlEQVQ4jaWSLQ4CMRCFXwkk3AU8HIMz4BCcYbIGgQSBIcjVNYg1GwRiMCiCR3EGgngI2k3Zdgk/L2kymfZ9M5kp4CSlWvwqKdWSpIdIrlZytaLbJFRlE+e9eS5PmJfozNbNJJmE7Jy5ULErd5ogPm6FF12cq3gyzEY+7uH2UmiYjUdoUqFS0UXXz3ZL+X7AfrD/mTdyDfP305QkeT9N6XMtAMil9geOGAAALjiE5k5/aYwxpt1boILkbvJ1SAQNVO8ieuyhqV1H5iap7D/vIFS4xncAkjGgcD8vBQkr/txBao1/6wGS+uH/ggiW5wAAAABJRU5ErkJggg=="
    },
    "ALL_NUKES": {
        "name": "Spells to nukes",
        "description": "Transforms every projectile currently in the air into a nuke; not a good idea",
        "meta": {
            "action_type": 6,
            "action_max_uses": 2,
            "fire_rate_wait": 100,
            "reload_time": 100,
            "speed_multiplier": 1,
            "action_mana_drain": 600
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA30lEQVQ4jbWSLQ7CQBBG3yQNopJmRRPwOByyKEiwuDU9QcMVkL0AnAGDweKoBIVCkdQQDCiSCkwRkA2EluUnjN2db968DPxYArAckQPUfI/pbigc5+B1UUqhtX4Z4ADspSdZNcy3Z1CKPNnUCRoKrbV8RACw9Sa0KxGL8wjAGuIAtCIkjuNryClltu7QbKa4rvsZQc338PuHh6bVWF76sU6YjXtk1dCsmCQJQRCY1awBNj+OLcDm522CIj8iglP08V5WKxrcmo6F80xA2THZKM3jt8dkCL49pieCMll/qwu25G/9rcH5lAAAAABJRU5ErkJggg=="
    },
    "ALL_DISCS": {
        "name": "Spells to giga sawblades",
        "description": "Transforms every projectile currently in the air into a giant sawblade",
        "meta": {
            "action_type": 6,
            "fire_rate_wait": 50,
            "reload_time": 50,
            "speed_multiplier": 1,
            "action_mana_drain": 100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA4ElEQVQ4jZ2TIRKDMBBFX5ieAUUHU1MThWJ6A86A50h4zsANOihUDKaGAcUlUrVpBihNWZXZzd9kX34AqJvWmmGy3pq/o25au24UolN109o803S9YZwX0iQGIM80+n5VvxpEVVkoES+vJ+O8AASJASK5cprExLcHaRKTZ3ozghkmlzPDZF19ndyb/WiP8jdVZaHMMNmuN1Rl4WqSAxwn4RY0pxwgnGTUqizUJVScZxowwMO9lPA7hrRqtOuVUJB7JlMi/AbJh7kXEXxM45spROwaCCTfTBtIIXH2QznxGeEb8GHpNpsWmbYAAAAASUVORK5CYII="
    },
    "ALL_ROCKETS": {
        "name": "Spells to magic missiles",
        "description": "Transforms every projectile currently in the air into a magic missile",
        "meta": {
            "action_type": 6,
            "action_max_uses": 10,
            "fire_rate_wait": 50,
            "reload_time": 50,
            "speed_multiplier": 1,
            "action_mana_drain": 100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABa0lEQVQ4ja2SzytEURzFP9/x/F6JaRbUlNBseOTHRqYXYyHKzmrK5i3YobFmRzRba+vZTiywmFIiqcFfMEoULxTjIb4WE83E8yhndbv3nnPPOfcL/4GH1Zg6VhDHCv6ZGwBwN47F6GyiatRUN7uCqv5awHCsoBqtIamY7FCA1+0tdUOvqKqIiL9AfeZKHFBj5xyAmsT2x5kCnwrtaz2aSeUEoD5zVRqhatSUQKwR6Q6Le7nM2/OZFJMBMqmcGJ1NWBPhkpgBgNr5HfKzm7ydXQNIWWWYYvuRxIgarSHm7Cfti+bp3Uvp/q2tqop/yCKR8uaC9ZPpw9IIfkSAo9CL9EXzHERM2b+1v40JQHpRPWcikhjR+/VxHi+WvF90rCB3M12kF1Xd7IpvtBILybiqXd0uOfNUAXLX0D8IdQN4zsSX3WRcta2lsB5bKLroofBZYjJe+Ngpc1jCDTAUteVmF++yfkIyrv5l/RfeAfv1kLFGiDCcAAAAAElFTkSuQmCC"
    },
    "ALL_DEATHCROSSES": {
        "name": "Spells to death crosses",
        "description": "Transforms every projectile currently in the air into a death cross",
        "meta": {
            "action_type": 6,
            "action_max_uses": 15,
            "fire_rate_wait": 40,
            "reload_time": 40,
            "speed_multiplier": 1,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABPElEQVQ4jZWSvUvDUBTFTxJFYq24CF0stGvp4uYQcAxF/4dQyV6nbvVjlA4WsgazZs7w1lqhuBcHlxQ6FewgfTEqml6np0kqbfLb3uN+nHM5OLoJAACGx2F4vAEA4i8X7UGAVt+nvH2S4XFsbnzi63vrQqspndFkKr/yfck5LWYaIANoFNQ53Z3sXDYrqlwvlxZ7xRcSdoA/S4bHYTp6L/73i+2Hke2H0Sob7UEAlyyWUCAmPT6HymgylW+Pq1K60fA4TDbDoeawA3zoQ+qS4fHlDXHZcUxH77lksSF1SeCSxYSdzIghaRtrETZNNlu6wb+F8VClySQ7T6gSFxehKqhzqpdLi4en6BrA1apQyal3QzQ3K6qs1ZROOlSZaPV9IiKy/TBaV5tQII749r4rnd+PcVbdVnJtjpNV9g8e3qm7FEgI/wAAAABJRU5ErkJggg=="
    },
    "ALL_BLACKHOLES": {
        "name": "Spells to black holes",
        "description": "Transforms every projectile currently in the air into a black hole",
        "meta": {
            "action_type": 6,
            "action_max_uses": 10,
            "fire_rate_wait": 100,
            "reload_time": 100,
            "speed_multiplier": 1,
            "action_mana_drain": 200
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA80lEQVQ4jbWSoU7EQBCGP8hJNKKtbS2qlTwBchWGOs7xBBtIeIJzaxCXHFXIurqTfYTqbp+gSeUgYDdtLzmWEH61mezM7vdl4I+5AFDaANir24cIYDzuh4/XbRw8RWlj0ziXNM5FRCSNc1Ha2JDeSwD38v3dE1lSMK8FDXDNVb3z59Bs4Iu5qt+irm/JkoKq3nFzPQ7uUpAjpY0tm0nKZjrhV9rY58eDOE+/cQRA2Uyyllw2kywchCZLioWjTQjneNwPQNT1LS/b9xNHnnP+xTXnOUeecy3KcZ7LwsF8D7q+DTAyG/DN6RcqSwpfC86PnP+RT3SjqTV5DEOsAAAAAElFTkSuQmCC"
    },
    "ALL_ACID": {
        "name": "Spells to acid",
        "description": "Transforms every projectile currently in the air into a pool of acid",
        "meta": {
            "action_type": 6,
            "fire_rate_wait": 100,
            "reload_time": 100,
            "speed_multiplier": 1,
            "action_mana_drain": 200
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAtUlEQVQ4jWNgQAK2fVr/GUgETMgcDQNWUvWjGqCrpEW+Aan79P8j07gAujeZ0BUQcgW6N5lgtsI02ssk4XUFugUoLrCXScLrCmzeZGFgYGCY7XSRMXUfw38GhnkMDAwMDHkKyxnxeQNiwUV8SiAAOcBS9+n/n/Qg8v+kB5H/L/7Z/R/mCoxARAbY0gW6N1mIdSqp3oQ7mVC6wOsFZKeSZAAp6QKvCwilCwYGBgacAYHsCnwBBgBrvUiSfuG9qwAAAABJRU5ErkJggg=="
    },
    "ALL_SPELLS": {
        "name": "The end of everything",
        "description": "You're heavily advised not to cast this spell.",
        "meta": {
            "action_type": 5,
            "action_max_uses": 1,
            "fire_rate_wait": 100,
            "reload_time": 100,
            "speed_multiplier": 1,
            "action_mana_drain": 600
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAApklEQVQ4jaVTURXEIAxL703EDKBiiJkCzMwAYjgVZ+Bc9H4GK107bm98QUKbQIBgDP5EtnAKhSy8K5TFnCNzFmvFA8AkyZFC5U97Lcudcjb4veZ1eaZ/xkh95OKxg0kDnCNj8Z3R2l+064BCIWwAtkH+OveKcRJ4OvLX7+J5Ck1RdW7K6XAlndZ1e1l4i46zo/QV80XdjfeBPMf+US4aWbwZz53v/APVb48XzNhh7wAAAABJRU5ErkJggg=="
    },
    "SUMMON_PORTAL": {
        "name": "Summon portal",
        "description": "Summons a strange portal",
        "meta": {
            "action_type": 5,
            "action_max_uses": 7,
            "fire_rate_wait": 80,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABuElEQVQ4y42TvWrjQBDHf+uI1G5CeuMiD7Bwy9kYF/sE1xlc+BHcy1ylIjl8IL9BikA6X3PlgoyIozXoAQIxeobUFx+TQpEtOfFxA1toRzv/j5lRIsJnsZp5ABlGRvGPaJ1KDCNz8tFuywFVRD6c69GNJGEmr88iSZjJ9ehGRIQkzCQJM16fD/8GtYoqHsdoa8QXKWfFOf2JIXceT8pqNhBAnZKgALQ1kjtPD4u2pQRtDT0suSs9AQi6n3jw8/sPWbgIgOndlNx5gq6QO8/0bgrAwkVsirThelBV/NIZcFaco60hHsescSyv7rlsX8C4ZIID3Wmaq5IwE4D+5JAIusLXK8O39ojlyz2PT57dtpT/cOvrnVIBoIaRYTXzkjuPtqVxl+0Lli8lg7lZ7O+1NQwjo97nhFbV702RssbtNfewPD6VhlaerHFsirQpoT6JFYvK/f7E8HDrqd8tXKR++d/NLsxNLHMTs3CR0tbsmQB75Orx8RwE9Tl4r6w2xUB0p0TX1vC3+LNH3m2R3RZVzULraIFkNfP4IlUV2jAyyhdpHbnJor4DSZiRhFn9W47yH3ZBnVrn/403y5obPWonfCsAAAAASUVORK5CYII="
    },
    "ADD_TRIGGER": {
        "name": "Add trigger",
        "description": "Makes a projectile cast another spell upon collision",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 10
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA+klEQVQ4jc1SOwrCQBB9fhAxWm/hVlYKQQQbb5Eb2GkTPIHHCGniMTxCmoBNCIHY2WwT2ygruLAWkmUNwURtfM0szLx5b3YG+GtkDpFVNc13yfZsUSny0kBXTGzD02NdR+C+JRPb8DKHSH7ayswhMrENj/tWNTlX5Ket5L4l9ag70dEqkum4v2r2OgA10SVTiJYEWIy+OM+XIwzdw32vcxr5bOrDqAkA6JKpKrql0fPBYgCACAMMNmlDNSg6aM8WysEtjQAWQ4QB2PGym7jXtc552cLEva7Z8bIDNQEWKzKoWUouxU9bKN5B3qwsXwt1VN+esgiDjwS/wgO3V5Ci7NGQ6QAAAABJRU5ErkJggg=="
    },
    "ADD_TIMER": {
        "name": "Add timer",
        "description": "Makes a projectile cast another spell after a short time",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAxklEQVQ4jWNgoBAwInOkzdP/C8gYMDAwMDAISJjAxT+8OAOhn1xgeHpyJooeJmQONs3IfJg8TrB8+fL/pMqjuOD06dP4bcACGNEFioqK/puof2cw1FNHEb/wQJwhMjISQz2GALoh5y/dZGDks8OqGacByIbg04wXwALs9evXeAMWr2YYIMkQXFFJlCHomq8fn0DQJUzoAjCwbGbmf3RD9uzZg6EOJXS1g6f/Z2DATMoMDIj8cHVtJu688OHJBRTF6Jph8lQFAKwmX1wywfJaAAAAAElFTkSuQmCC"
    },
    "ADD_DEATH_TRIGGER": {
        "name": "Add expiration trigger",
        "description": "Makes a projectile cast another spell upon expiring",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAmklEQVQ4jWNgGGjAiE3wxr1X/9HFNJTEsKrFEMSmGZ8hKAL4NOMyhAmboiNnbjAcOXMDg40NsKALhEQmwNk2yxcwTOjtYGBgYGCYwMDAsGb5AgwDsLqAbDBn1aH/N+69+q9j7vX/xr1X/7HxiTZkzqpD/+294v8j84nWfOPeq//2XvEoNC5DKE5IOAMRpgGXRrwGIKcBZHpwAgCqE3SE8v7dTwAAAABJRU5ErkJggg=="
    },
    "LARPA_CHAOS": {
        "name": "Chaos larpa",
        "description": "Makes a projectile cast copies of itself in random directions",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 100
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAbUlEQVQ4jWNgoBX4////f5Lk0AXfncFuAC5xrJLLMo/g5WMFyIrKXfuxsgkCkhRTDaD7H8b/DwUwl2GEAy6NMIBsALIafNFMHUBsDLBgE1yWeeR/1HQbRrJtx5baiEpEDAy48wDeJEwsoEnIAwBoy2zjz99rbwAAAABJRU5ErkJggg=="
    },
    "LARPA_DOWNWARDS": {
        "name": "Downwards larpa",
        "description": "Makes a projectile cast copies of itself with a downwards trajectory",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAeklEQVQ4jWNgGFRgWeaR//j47878/8/AwMDwHwrgEuWu/URphAEMA/ABZMPRLSIZoLuMgYGBgYkcTVgNwOW8i3dOw9keydaE7MM0DNkFRAccsib0GEAHBL0gaIzfMqyBiOxXRkZGRqIM0FcxJdpWrICQXwkCokOY2gAAeFdNpVAAlZ4AAAAASUVORK5CYII="
    },
    "LARPA_UPWARDS": {
        "name": "Upwards larpa",
        "description": "Makes a projectile cast copies of itself with an upwards trajectory",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAdklEQVQ4jWNgoBb4////f3L0McEY78+SZzHcgB1zj5LlGrgBF++chguS4hombILkugYOlmUegWt6dwa/AVhdgOwdol1T7tqPVRJZnJBrsAJk7yCzyQLYDMAaBsgA2QvIYUOUDeh8WBj8hwKstuDTCAMYBgwoAADDi1zhLEueuwAAAABJRU5ErkJggg=="
    },
    "LARPA_CHAOS_2": {
        "name": "Copy trail",
        "description": "Makes a projectile leave a trail of copies of itself",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 150
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAlUlEQVQ4jb2RsQ3CMBREL2yQWdLSeolsYDoqz0Dlkg2cIWhNxW/omeGvcBToR5ESCdtBXP/u7v8D/qngIlVIFbIZNpnJ4ec1t5R8pgq5PCG4SADoSgxUyH4AptMdz9cDAHC5nb+zwUUmn7lsUFzbYHtYNazC/cnJ5/bkahj4zFV78zxF8nkGxuuxaN5dySuDZtjUCr8BaLKowXIBA8MAAAAASUVORK5CYII="
    },
    "LARPA_DEATH": {
        "name": "Larpa Explosion",
        "description": "A projectile will shoot out 8 copies of itself when it expires or hits the ground",
        "meta": {
            "action_type": 2,
            "action_max_uses": 30,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 90
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA0klEQVQ4jb2SoQ7CMBRF3yX8AENgEXjEhsOSkCDw/MEkim9AIfcH8wgSktk5NoFHYCc2PuFi6NZ1W1rFca/tPXmvLcSBaOfznr0769eixGgoRJI28X42ZS0wA5/cpTeRWgAAVdZIvACIw9TaRWsEM/R8Pdw7UByiNU6bC0VEzskRNoH1gGLoJeoO9Pn1mj/CW45tMO8K1EEvAPSgqv83go66RFu4VxCHKdXtm6I+xnpRZaQ++3KxEkkcBSQJNGFTNkTrK+sbE98WNQQmprCPa1HiC2UKZG8/NjczAAAAAElFTkSuQmCC"
    },
    "ALPHA": {
        "name": "Alpha",
        "description": "Casts a copy of the first spell in your wand",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAi0lEQVQ4jWNgoCX4eaUgn5AaJgLykT+vFJzAp4CFgAHmZLsAavM0JDZpBkBtP0TIJVgNgNoYwa4zYSUDA0MEIVdgM+A/Oh9dDKcLYLajCeN0BSMh29EBu84EDD0otv+8UhCOQy4c6hXcYUHIdmxhwYQkic3v6AB3jBCyHZcrmJBNJBTXyPIkpQt8AABkjV9d13iXpgAAAABJRU5ErkJggg=="
    },
    "GAMMA": {
        "name": "Gamma",
        "description": "Casts a copy of the last spell in your wand",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 15,
            "speed_multiplier": 1,
            "action_mana_drain": 30
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAY0lEQVQ4jWNgoBAwInN+Xik4wcDAYE5Az0l2nQkWMA4LmmQ/AwPDCgYGBgZ2nQmMaHIMP68UhMPkcYKfVwr+/7xS8B+P/AlkPhMB52IAZOeTZQA6GDVg1AAMA5CTKXqSpRkAABqEGtWCBTIKAAAAAElFTkSuQmCC"
    },
    "TAU": {
        "name": "Tau",
        "description": "Copies the two following spells in the wand when cast",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 35,
            "speed_multiplier": 1,
            "action_mana_drain": 80
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAd0lEQVQ4je2SuxWAIAxFHzQswz4wRtyENdhE+iyTCis9iODBT2HhrUhy8rhFgIcoABCmfGfZ2KB0UScA3tigjA0KgC9mdT/tkoRpFiZX/yBMuWUnTG7tbwbGhnhBPR4sWvQMSvTZcIQ/4NMB5WW2rnTEYOq832UB2s0sHjPyVxgAAAAASUVORK5CYII="
    },
    "OMEGA": {
        "name": "Omega",
        "description": "Casts copies of every spell in your wand",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 50,
            "speed_multiplier": 1,
            "action_mana_drain": 300
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAuklEQVQ4jbWSsQ3DIBBFv61ILMMM9GQal2ECXDKN6d2m9TKukuZDDgwSUZTfAMe7f3fYwI+aehfnsWwALI9R6XAfMqgSa12M5k5yBOCUDpPSYQLgGLNkrgbnsTxE5V3psKY77nceLdlLB0a0uaISY7FiC4Pe3C1lVhrEBthTZqVBnnGgcmI/BpzxCeSvUUjEijdq/Qcvbl0C+eqehYqcuZXI1QjOyDvB4tap3pM/j8U3OwDnH9Q37J/1Boj+SifrBGGoAAAAAElFTkSuQmCC"
    },
    "MU": {
        "name": "Mu",
        "description": "Every modifier-type spell in the current wand is applied to a projectile",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 50,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAqElEQVQ4jaWTwQ3DIAxFH72wDPdkkzJGugljpJs09yyTU3shyCG2Gym+YGTr/f+RgJsVALZ1+gADsMRURmt5W6cnMNfrElMZgxh+a5tjKm8DsO8QUwkAD2Xv5aifSgMM2mIF53+A7Kip4ANAZD/E2IHa22gRNDXVvgXQYljvcgb0MTz7loNe1bTvAWQM074JEHbn7n7ZQXOBY78B6mdqp1SV6nIu+1v1AzqGQV/ImNhsAAAAAElFTkSuQmCC"
    },
    "PHI": {
        "name": "Phi",
        "description": "Casts a copy of every projectile-type spell in the current wand",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 50,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAvklEQVQ4ja2SsRWDMAxEf2hYxj3ZgBHCGGaCvEwAY5ARskHcexlXpJH9BNg8eOQqo7NOOnxwEbcSEbz9Ao18utqM99Pqwds5eDuf2iB4+wB6NT3CAUNtxrcuVpnmCaA2YxJX50nubAVUc9av1NxapJLmVtYGeG6dJkSul56FhUYmfUrdikv/pyrcPQwt4CDZyUJxbiEgqw1Se+0MjNwQ7aQN5H07oJEUrqfHZHY6C5eDtIsjUS6+graRs/Q3/AD5XFYQZidrzQAAAABJRU5ErkJggg=="
    },
    "SIGMA": {
        "name": "Sigma",
        "description": "Copies every static projectile -type spell in the wand when cast",
        "meta": {
            "action_type": 5,
            "fire_rate_wait": 30,
            "speed_multiplier": 1,
            "action_mana_drain": 120
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAr0lEQVQ4jaVTwQ3DMAgkVSUv43+6SbpF6SRxt2g3sf8swyt9FCQ3BWop97ERcJzlA+Agpj5gwgoA80hjymUyE0xYmXBjwsXJL0y4aXwyalY5787kFwA0jU0Z3VNayuViSo0IhERlXmWqCesJCpTzGSlwCVIuj6hxRIGiRUmXgAlvcl29mhDihfqvzlQgjc2b3hOfjeQCHw+Y37d36I8PehMFymfdhS8Fu2UaWqrDeAP/XkOcjzIOEQAAAABJRU5ErkJggg=="
    },
    "ZETA": {
        "name": "Zeta",
        "description": "Copies a random spell in another wand you're holding",
        "meta": {
            "action_type": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAApklEQVQ4jaWTyw3DIBBEnyNLNMM96STuIqSSUIbTiXOnGU7OBUsIBkyUufDZ3ZmBBfgTU76Iwe2DdR9j/Q1gFsHFWP8uN2Nwd2DNiwEugrkqTnim8ZVvTiKxQgxuA66l+hBBZh1jfZVfHkFhTaNTwa6DM/URB4f60kpoEsTgHse805muA3+m3iRIbYP+u9AE6eJkz4WAdCBfnBAA6s+05cEeWm39GV++YzfEQwsUIAAAAABJRU5ErkJggg=="
    },
    "DIVIDE_2": {
        "name": "Divide by 2",
        "description": "Casts the next spell twice, but with reduced damage",
        "meta": {
            "action_type": 5,
            "damage_projectile_add": -0.2,
            "fire_rate_wait": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 35
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAQElEQVQ4jWNgGAjw+vXr/69fv/7PwMDAwDQgLiAbwJw9BDQjhzIhzSzoikRFRRmRFaDzSQYU+X2YGgIzaPDkBQAp+jjJq/ISzwAAAABJRU5ErkJggg=="
    },
    "DIVIDE_3": {
        "name": "Divide by 3",
        "description": "Casts the next spell 3 times, but with reduced damage",
        "meta": {
            "action_type": 5,
            "damage_projectile_add": -0.4,
            "fire_rate_wait": 35,
            "speed_multiplier": 1,
            "action_mana_drain": 50
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAARElEQVQ4jWNgGAjw+vXr/69fv/7PwMDAwDQgLiAbwJw9BDQjhzI2zcjyLOiKREVFGZEVw/gwNejy1Hf+CDAEZtDgyQsAGtdESdyrg+oAAAAASUVORK5CYII="
    },
    "DIVIDE_4": {
        "name": "Divide by 4",
        "description": "Casts the next spell 4 times, but with reduced damage",
        "meta": {
            "action_type": 5,
            "damage_projectile_add": -0.6,
            "fire_rate_wait": 50,
            "speed_multiplier": 1,
            "action_mana_drain": 70
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAR0lEQVQ4jWNgGFLg9evX/1+/fv0fWYyJbjZTVTOyVwh6QVRUlBFdE1G2whQT0siC7kyYjVQHpLgKQyM+w8g2hCwwcCmRpgAAZ7FWy1BCuMUAAAAASUVORK5CYII="
    },
    "DIVIDE_10": {
        "name": "Divide by 10",
        "description": "Casts the next spell 10 times, but with reduced damage",
        "meta": {
            "action_type": 5,
            "damage_projectile_add": -1.5,
            "action_max_uses": 5,
            "fire_rate_wait": 80,
            "reload_time": 20,
            "speed_multiplier": 1,
            "action_mana_drain": 200
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAWElEQVQ4jWNgoAZ4/fr1/9evX//Hx8YFmMi1iGKXYpXERqOzibYBm0FEuYRYw/EaQIpiFnRnioqKMpLkNGIBUa6iKBCpEo2keAMGmLA5nZS8wEKKC2gSQwCeBr/Np5K1zQAAAABJRU5ErkJggg=="
    },
    "METEOR_RAIN": {
        "name": "Meteorisade",
        "description": "Alea iacta est",
        "meta": {
            "action_type": 1,
            "action_max_uses": 2,
            "fire_rate_wait": 100,
            "reload_time": 60,
            "speed_multiplier": 1,
            "action_mana_drain": 225
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAwUlEQVQ4jbWSMQ6CQBBF/xJDNLGx8A5S0hpuYEPNFfAeNlvqPTwEveV6ByoqDM23kZWd7G7EhFeR2WH2DR/gQ6crtmVOROh0RZLsTW17k9gLkvS0AwCsD1dsL8fvQVvmJElpIY0osAZpkTk3jOzvD/WTWm9qO7U3ddAg1OdVGwf4hnS6srVVzCwtMgyNcWqb7OaslQDA63m2henzLKRaKBmJXUGqTZMZmj+NvF9cEMx5mgYAKKW8vbN+5VkDFklmEd7oL6hoU35ehwAAAABJRU5ErkJggg=="
    },
    "WORM_RAIN": {
        "name": "Matosade",
        "description": "Alea iacta est",
        "meta": {
            "action_type": 1,
            "action_max_uses": 2,
            "fire_rate_wait": 100,
            "reload_time": 60,
            "speed_multiplier": 1,
            "action_mana_drain": 225
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABi0lEQVQ4jZ2SMUjDQBSGv8soqGsU4iAI0WzSQqFYIe2axaGDzdCpg4OdCiq6CXatY1w6VAeHInZysBTBRcRFUgsFBwXNqoWu5yAXakyl+Jb/ePC/+993JwBs15PDoM+UvoRSgLieqveHFgCa7XpSt0wAlI6eozoM+uiWydyqw1O3JbR2oyQCv8di1uF0Zy28SfWq2Q6LWYfA75FfqHKydUdmUAwHiuUVRwLMrTq/4kV754eflJsFahunlJsFhkEfoRjolsnzdSu8LY5BfqFKOmFwe//KzXSds92M0NR+mUExjDeOwfnLDvn9GWoXFtVsB9v1pJbKVWTg90gnDMrNAumEgW6ZjHJRqYZBn65/yey8iZE8oN0oCQGweXQjM4NiGK92YcUyAOj6lwjx7QMQtuvJ+nZAfm/Ax9v37lf1JMVjnSiXdqMUGlVp7UZJGMmDcLqKF+Uy+kfGlpRSAqRyFWm7nnxsrv/QVK4io55fkVRFuahnm2jAX1ziOIxdJ5WryFGdyBg36F/GSesLD6PmXky9oZUAAAAASUVORK5CYII="
    },
    "RESET": {
        "name": "Wand Refresh",
        "description": "Reloads the wand immediately",
        "meta": {
            "action_type": 6,
            "reload_time": -25,
            "speed_multiplier": 1,
            "action_mana_drain": 20
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAe0lEQVQ4jWNgoBAwYhP8/+aRPgMDw2o04VBGEbmLeE37/+aR/v83j279f/OoBYtcC1ROH69mQk7GaQgxmrGpZYLZzsDAsIpYAxgYGFahuIIU29H1MJGqER0MHgNCscU9LgBVG4ouSH40IrmCqISEYTuSJMlJmbqZiRwAAKwoa5/Mp4cVAAAAAElFTkSuQmCC"
    },
    "IF_ENEMY": {
        "name": "Requirement - Enemies",
        "description": "The next spell is skipped if there are less than 15 enemies nearby",
        "meta": {
            "action_type": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABT0lEQVQ4jWNggAKTrTfyGXAAbHLKzXM9UBTMfPn/PzaF2OSUm+d6RG+/+B9uCC4DiNIMU/j/P6pCbGJYNWPTgMxGNkS5ea7HzJf/UQxgQg4k0203C057qU9gYGBgmPUKogBGKzfP9bAwMdneNWue593a5B0wA1hMtt7ITzWBaEoTw3AYw+wzNwtSTdQnzGZgKLAzUmc4ceYMijzLGW+NiQxbb0AUo2k8460xkYGBgYFh6w2G9+eO3mQwUce0AT0c7r848P/+iwP/j1+fhBGA6P6HhwFM82rjFxNgfAlBPYbJSrsm4EtgKAYgg4SUWIYX7y8xVJR2o4gfOneJOAMWzFnMsGz1QoaO7lK42N3a5B0nzpzxLEtL2o4RjQwMDAxnvDUmhp6VKHjxHmKLkaUCVidjcwUKMNl6Ix8WgOgBycBAIDUiG4JMowOiDCEEYJoBhaXrnFGgxvUAAAAASUVORK5CYII="
    },
    "IF_PROJECTILE": {
        "name": "Requirement - Projectile Spells",
        "description": "The next spell is skipped if there are less than 20 projectiles nearby",
        "meta": {
            "action_type": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABcklEQVQ4jY1SPWjCQBh9KQ6dugiFQrcMyVRpSaFTtkJUKJRO0lHs0MWAs1O2QiGBUlBJ15ukdEh1DghSD0WnOEidCnYp2M3lOsQLiZ6hH4Qc3/vJ4+WA9WheUMWOEWGy5RoJQmPBmIgowmTLNW47YxaZ7DL4l5gTGUsSRTuhWCSIn+MmsuUajQVLGOzFSzp/n5qDgmIDQPM7JPC3bLnGhaZ1Hpov+Vm93OUGGc0LqhUtFN0dbgVDi07NiqbYLcDUzxT0KU3gGVpUHXhBSF4v+Y4WVQcA4AX4Gfam0JStD+xFgo2J70R4lIB38Pyp2gBwfJTF1ToRANSWI6dUKu3Shwa0qDr3XoDacmTPVkBtNbJ934eu63g8OAUABwD84UTsonlB9eMJjD+EEPbVzjJCCCOEpP7GRIKbyasNAPid4218iVxujvbJtRnvYDOFJOxgv2dywqCgOJIkRQJ+H/qU5mf1cjdC4lc2rfVNkzRe6vAe/gDZX/vCq8cTWQAAAABJRU5ErkJggg=="
    },
    "IF_HP": {
        "name": "Requirement - Low Health",
        "description": "The next spell is skipped if you have more than 25% health left",
        "meta": {
            "action_type": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABGUlEQVQ4jZWSMU7DMBiFP1U5AxdIpXRGRmJiDu0JECMqa7NHTOnElI5tFRYGThDKjISEhFUJpkQRBygLRzBDamQnTqBvsfX/7z0/2z/sIfJiRgdcPT/JQouw3CnlIrp6fpKFl5t39WvSZfAvsSYqZRNdNafYJTD3pomfZOFypyyDgflIJ49l9DYOUoDVV03Qq59k4akQm9vV3fnnzdWTNvBEXsymohZdH7WCsZZlNBVBuobo7DjgVUqr78nJaEFe1OR9UdfkZLQAIC/43r6UiKB9gustuv69eX8Aryl+GN+nABdmgh4MTDHAkDlD5lZN43n70R27UrFqolJxaw6c33gIelPowTEHqsn7cxorFbeiH2zSJzZNAH4A/WP9Na4Gp/gAAAAASUVORK5CYII="
    },
    "IF_HALF": {
        "name": "Requirement - Every Other",
        "description": "The next spell is skipped every other time this spell is cast",
        "meta": {
            "action_type": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABMUlEQVQ4jZVSMW6DQBAcWbyBD1Cc6+iiuEoN9hPSxYLWVOlc0bnJuQQLt8kHCK4jRbKUk6WkgsIPIE2esCnI2dxxRM5Kp0O7M8Pu7AG/wYtqgYGw1bwk9zVA2hDZgLaal+T+XflBJ5EhgYvICkikA205K9lG6H53Rbwk99OGNIFR16Trlzp+nzIBANlXC1C3l+T+hPNylW2D43K+UwIOL6pFyFtS5PYaw0bWcciZ2ADx7RXDXkqt7sjZeI2iasEGUc7GawBAUeH78FaDs/4fTB/ShoiIyOa+Of/JA0UOOROrbBtMxDMiFwg5E389MABwTHK3GLkAOBNqhNfDZ09gpIEtofLH5Xy3lzJ4iO7L3hqB87rMMPO2LqDGUCbdPD5ZjQQueI1DW/iXiDr2Xs8iAPADNvQE2ysznoMAAAAASUVORK5CYII="
    },
    "IF_END": {
        "name": "Requirement - Endpoint",
        "description": "Any Requirement spells before this will skip all spells between them and this spell,",
        "meta": {
            "action_type": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA40lEQVQ4jWNgYGBgUG6e68FAAJhsvZGPVUK5ea5H9PaL//EZYrL1Rv7Ml///k2UIsmaSDTHZeiP//3+EJnQ+AwMDAxOyIXZGerh8QRzA5QqivAAzYObL/xhhgU8zCy7D0BXPPnOzAF38jLfGRKwGCBpZq5/2Up+ALGa67WZBqglELE0MKrb1Bm4XwDQxMDAwwAyDuWI2khgTuqZD5y5hGDTrFcLJZ7w1JmK1DTkA8YU8elpAcQE222efuVkAs9Vk6438VBP1CTCvodiOHP+4bIaJobgCVz7AFufoBqK4AGvAEAEAxFXVaZMzfA0AAAAASUVORK5CYII="
    },
    "IF_ELSE": {
        "name": "Requirement - Otherwise",
        "description": "If a Requirement spell before this succeeds, the next spell is skipped",
        "meta": {
            "action_type": 5,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABK0lEQVQ4ja1TMWrDMBR9CbmAl14ggzJlECoUDJnT5ggdi7vaF+iUCyijI3IMN50NhkCFoJmsoQdwF8+d1KGV+XaitpS+6aP//vufxxMAYLreLTHAdL1b5o1zeeNcqN8Vt/uXHkkUdZo3zomiTmlNh3szIRH3he+GR7RxJcT+oPX168PdEx0CAL2abYYcAJhQ0oLPcdAaoqjTRDB5f/H5vn0DUNRoTWU9x2NEBeiGiMcMABLBpNI2a01lh9sBYDx0d8Hn3ckAoLTNEsFkxGPmexQnAtTE5xsmvUhrKnuOFxQAgMtHm3mRECbnHiMeM2piIphUQPYrgdIc0RptFZAp9E0sx+/hU3x0fUBCCRxGu3dBaY5drVezDYr6ZBHl9Lb/Ncr/95lCnvz0nT8AuqQDB9LjpOUAAAAASUVORK5CYII="
    },
    "COLOUR_RED": {
        "name": "Red Glimmer",
        "description": "Gives a projectile a red sparkly trail",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -8,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAl0lEQVQ4jbVTsRHAIAhESxsLaxbIBtm/yAZZwNrCxtZU5AgaQnKXr+R4ngcV4G9sIXQzuSL2iqgWSI7nyb2Uk8QLZmeCm1leUgIAgJizk0XUZG3NDQ544q6j5DySJWLOF9eeJ8iehorY+c2oO7A4GASoi+aCiwxLpGI5DsXqmDQfichYfZV8OVzAMtIAKSQ7m//Hq4/0FQf4yGZBFbhPdgAAAABJRU5ErkJggg=="
    },
    "COLOUR_ORANGE": {
        "name": "Orange Glimmer",
        "description": "Gives a projectile a orange sparkly trail",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -8,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAoUlEQVQ4jbVT2w2AIAysDKImLGLCCo7BOIzBCiYsQqIuUr9KSpWHJvYHCndHDyjA33FYjd1g9AbRmypBYhTfPMOeQJzwNH8UmFwcSiI8JwwAwFAqs2ThDDtMLiZemrS8J8K6ZYdmyWE1jsvcFOFV3Cy0RKoVULTscBElN4nMb5rncj0TQG+QAOSR5/IFbifTt6Uq5NgdUkj2Q3d/vGqkr3EBsE53O1WBQF8AAAAASUVORK5CYII="
    },
    "COLOUR_GREEN": {
        "name": "Green Glimmer",
        "description": "Gives a projectile a green sparkly trail",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -8,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAqElEQVQ4jWNgoDXwnqr3n2jFtZej/tdejsKrAV0NE7LkuUNX4IqQNWBjwwAjNicb2ekwMDAwMDTrLmPEpuncoSsMW7MvMWI1AJdN2DQzMDAwsBCjCRkga8ZwAbLz8QG8XiBkSLPuMhQ9TOgK0J2IDtC9imEATAEsSpGdjU0cxYDay1H/YQpgLkHmo8cAigEwzegKkPmEvAcHsPQP8w56fiA6f5CUkcgFAKrSbKHTPb5vAAAAAElFTkSuQmCC"
    },
    "COLOUR_YELLOW": {
        "name": "Yellow Glimmer",
        "description": "Gives a projectile a yellow sparkly trail",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -8,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAnklEQVQ4jbVTsRGAIAyMLGLLBu5hJRUzMIkzWGHFHm5AyySxChcjIHpnquSS/+QDAfjb/KqxuxiTRUy2CZA1iif3cOQiDij5RQLj4lAj4THVAAAMtTFrEvZwgHEx47LzpD0Dxu3SVPEEH61mmCzyl7lJ8KvGZZ66J3i9A0miZJLAUg7FTZmkj0hk3PyVfDmcoEfSzSSR7Nx9H68O6audYXmAA8HEw7sAAAAASUVORK5CYII="
    },
    "COLOUR_PURPLE": {
        "name": "Purple Glimmer",
        "description": "Gives a projectile a purple sparkly trail",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -8,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAn0lEQVQ4jWNgoDXoltj5n2jFB9X+/z+o9h+vBnQ1TMiSpz7tgitC1oCNjdWA0hfujLgMQebD1DAwMDAw4nImLi+c+rSLofSFO1wfnEHI7zBgf4sRxVImZAlkp+ECB9X+/0eOGQwvdEvs/G/G50a0C0gOA3RDmNAlYZrRvQPj4/UmzH8wQ9D5eFMlcuAgG0CMlzAAukHoNhOdP0jKSOQCAJsTe14+YBomAAAAAElFTkSuQmCC"
    },
    "COLOUR_BLUE": {
        "name": "Blue Glimmer",
        "description": "Gives a projectile a blue sparkly trail",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -8,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAoklEQVQ4jbVTuxWAIAwMzGFpZSUbsIsjMIcjsIsbaGVFyR6xCi9E+eh7XgOBy5EDAvA3BrdjN9l6ROuxmiA5mm+G80gknvA0fxSIq1ElER4TBwBAlcosWQjnAXE1KS9NWt4J26KyQ7NgcDuO09wU4VXcLLREqhUQWna4iJablMxvmsdyPROwHpEI5JHH8gVuJ9O3pSrk2A0pJPuhuz9eNdJXXMIKdeU5OeOPAAAAAElFTkSuQmCC"
    },
    "COLOUR_RAINBOW": {
        "name": "Rainbow Glimmer",
        "description": "Gives a projectile a randomly colored sparkly trail",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -8,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABE0lEQVQ4jWNgoDXwnqr3n2jFH2Vl//9f44xXw0G1//8Pqv2Hq2FClrz29i0DAwMDA7IhtZej4GyHuf8xDEcxwPLbN8ZHBx5ADHkYj2IIsuZTn3ZhN4CBgYFBfspdRhgb2RBkzaUv3OFqWGCMj7Ky/xkYGBj4+tXQ9aAAZM0oLuB//JgRFgb4wEG1//+7JXbCXcaCLGn57RvjwwMP/ss5KOA0wP4WI3YXwAByGOByAV4DYFG4Yt1JFPE71y4wMDCgxgCGAR9lZf/DojGy8AYjAwMDw7lDVxgYGBgYnvQaMt65dgEjEFE0H+fi+o/sClg0IicmdIASC5bfvjEyMDAwwFwBAzBXwAByLOAFy/s1iM9I5AIAVPN98thLWs8AAAAASUVORK5CYII="
    },
    "COLOUR_INVIS": {
        "name": "Invisible Spell",
        "description": "Turns a projectile invisible",
        "meta": {
            "action_type": 2,
            "fire_rate_wait": -8,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAZklEQVQ4jbWR2w7AIAhDwf//5/qwLCFNy9ycfVI4XFIiTgsAXsFPBS1zJyqg3u0QhlCkmi5v4orTJjIzSACg4ra7Y5Y8+Dpgv4kz0MVTQRGXierPJg4uZqD+1QVGTbYnMpv+A+5oAk1jl5e1IUUwAAAAAElFTkSuQmCC"
    },
    "RAINBOW_TRAIL": {
        "name": "Rainbow trail",
        "description": "Gives a projectile a trail of rainbow",
        "meta": {
            "action_type": 2,
            "speed_multiplier": 1,
            "action_mana_drain": 0
        },
        "sprite": "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAADMSURBVHjaYvz//z8DJQAggJgYKAQAAUSxAQABRLEBAAFEsQEAAYTXAJfLTP+Z99n8Z9yw9j9jzX1YaP+XY5wCD3mAAEIx4LebBlji32VeuIK/TkcYYWxGq23/uTimMzz6nwPXAxBADKBoxIaPvgVR/xmY9lr/x6UGhAECCEzcZWZGKOpXwasBHQMEEArnl6s6SZpBGCCA4IxX0sJwzX8v8YDZzpcYCRoIEECMlCZlgADCGgukAIAAwuosmBeIwQABRLEXAAKI4qQMEGAAqRgM28UbxBMAAAAASUVORK5CYII="
    }
};
