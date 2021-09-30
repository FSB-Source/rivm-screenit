/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

export function getMandatory<K, V>(map: Map<K, V>, key: ?K): V {
    if (map && map instanceof Map) {
        if (key) {
            const val: V | void = map.get(key);
            if (val) {
                return val;
            }
            throw ('Map didn\'t return an expected value(-type)');
        }
        throw ('Not a valid key');
    }
    throw ('Supplied map isn\'t valid');
}

export function getIfExists<K, V>(map: Map<K, V>, key: ?K): V | null {
    if (map && map instanceof Map) {
        if (key) {
            const val: V | void = map.get(key);
            if (val) {
                return val;
            }
        }
        return null;
    }
    console.error('Supplied map isn\'t valid: ' + map);
    return null;
}
