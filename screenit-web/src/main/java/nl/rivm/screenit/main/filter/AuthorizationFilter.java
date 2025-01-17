package nl.rivm.screenit.main.filter;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nl.rivm.screenit.main.web.ScreenitSession;

public class AuthorizationFilter implements Filter
{
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException
	{
		var httpResponse = (HttpServletResponse) response;

		if (!ScreenitSession.exists())
		{
			httpResponse.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			httpResponse.setHeader("Content-Type", "application/json");
			httpResponse.getWriter().print("{");
			httpResponse.getWriter().print("\"message\": \"ScreenIT session niet beschikbaar.\"");
			httpResponse.getWriter().print("}");
		}
		else if (!ScreenitSession.get().isSignedIn())
		{
			httpResponse.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
			httpResponse.setHeader("Content-Type", "application/json");
			httpResponse.getWriter().print("{");
			httpResponse.getWriter().print("\"message\": \"Gebruiker niet ingelogd\"");
			httpResponse.getWriter().print("}");
		}
		else
		{
			chain.doFilter(request, response);
		}
	}
}
