package nl.rivm.screenit.main.web.security;

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

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.security.Constraint;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.subject.Subject;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

@Slf4j
public class SecurityConstraintInterceptor extends HandlerInterceptorAdapter
{

	@Override
	public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception
	{
		if (handler instanceof HandlerMethod)
		{
			HandlerMethod handlerMethod = (HandlerMethod) handler;
			SecurityConstraint securityConstraint = handlerMethod.getMethodAnnotation(SecurityConstraint.class);

			if (securityConstraint != null)
			{
				if (securityConstraint.recht().length == 0 && securityConstraint.altijdToegestaan())
				{
					return true;
				}

				var authorized = checkPermissie(securityConstraint);
				if (!authorized)
				{
					response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
					LOG.warn("Unauthorized toegang voor request {}", request.getRequestURL());
				}
				return authorized;
			}

			return !endpointNeedsSecurityConstraint(request);
		}

		return true;
	}

	private boolean endpointNeedsSecurityConstraint(HttpServletRequest request)
	{
		var isApiEndpoint = request.getRequestURI().startsWith("/api/");
		if (isApiEndpoint)
		{
			LOG.warn("Geen SecurityConstraint voor request {}", request.getRequestURL());
		}

		return isApiEndpoint;
	}

	private boolean checkPermissie(SecurityConstraint securityConstraint)
	{
		Subject currentUser = SecurityUtils.getSubject();
		var permitted = false;
		var constraint = new Constraint();
		constraint.setActie(securityConstraint.actie());
		constraint.setToegangLevel(securityConstraint.level());
		constraint.setBevolkingsonderzoek(List.of(securityConstraint.bevolkingsonderzoekScopes()));

		boolean isAny = securityConstraint.required().equals(Required.ANY);
		for (Recht recht : securityConstraint.recht())
		{
			constraint.setRecht(recht);
			permitted = currentUser.isPermitted(constraint);
			if (permitted && isAny || !permitted && !isAny)
			{
				break;
			}
		}

		var inScope = ScreenitSession.get().inScope(securityConstraint);
		return permitted && inScope;
	}

}
