package nl.rivm.screenit.security;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import org.apache.commons.lang.StringUtils;
import ua_parser.Parser;

public class UserAgentUtil {

    public static String getParsedUserAgentInfo(String userAgent) {
        var client = new Parser().parse(userAgent);
        return String.format("Browser: %s %s.%s, OS: %s %s%s",
                client.userAgent.family,
                client.userAgent.major,
                client.userAgent.minor,
                client.os.family,
                client.os.major,
                StringUtils.isNotBlank(client.os.minor) ? String.format(".%s", client.os.minor) : "");
    }

}
