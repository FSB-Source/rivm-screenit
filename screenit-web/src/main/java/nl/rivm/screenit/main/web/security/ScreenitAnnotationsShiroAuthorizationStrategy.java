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

import java.lang.annotation.Annotation;
import java.util.Arrays;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.security.Constraint;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.authz.Permission;
import org.apache.shiro.mgt.SecurityManager;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.apache.wicket.Component;
import org.apache.wicket.authorization.Action;
import org.apache.wicket.authorization.IAuthorizationStrategy;
import org.apache.wicket.request.component.IRequestableComponent;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.request.resource.IResource;

@Slf4j
public class ScreenitAnnotationsShiroAuthorizationStrategy implements IAuthorizationStrategy
{
	@Override
	public <T extends IRequestableComponent> boolean isInstantiationAuthorized(final Class<T> componentClass)
	{
		Annotation fail = checkInvalidInstantiation(componentClass);
		if (fail != null)
		{
			if (LOG.isTraceEnabled())
			{
				LOG.trace("Unauthorized Instantiation :: component={} reason={} subject={}", componentClass, fail, SecurityUtils.getSubject());
			}
			return false;
		}
		return true;
	}

	public <T extends IRequestableComponent> SecurityConstraint checkInvalidInstantiation(final Class<T> componentClass)
	{
		SecurityConstraint fail = checkInvalidInstantiation(componentClass.getAnnotations(), null);
		if (fail == null)
		{
			fail = checkInvalidInstantiation(componentClass.getPackage().getAnnotations(), null);
		}
		return fail;
	}

	protected SecurityConstraint checkInvalidInstantiation(Annotation[] annotations, Component component)
	{
		SecurityConstraint fail = null;
		if (annotations != null)
		{
			for (Annotation annotation : annotations)
			{

				if (annotation instanceof SecurityConstraint)
				{
					SecurityConstraint constraint = (SecurityConstraint) annotation;
					SecurityManager sm = ThreadContext.getSecurityManager();
					Subject subject = SecurityUtils.getSubject();
					Constraint permissieConstraint = new Constraint();
					permissieConstraint.setActie(constraint.actie());
					permissieConstraint.setToegangLevel(constraint.level());
					permissieConstraint.setBevolkingsonderzoek(Arrays.asList(constraint.bevolkingsonderzoekScopes()));
					if (component != null && component.getDefaultModelObject() != null)
					{
						permissieConstraint.setCheckScope(constraint.checkScope());
						if (permissieConstraint.isCheckScope())
						{
							if (component.getDefaultModelObject() == null)
							{
								LOG.warn("Check scope enabled, but compoment model was null. Component class:" + component.getClass().getName());
							}
							else
							{
								permissieConstraint.setScopeObjectClass(component.getDefaultModelObject().getClass());
								if (component.getDefaultModelObject() instanceof HibernateObject)
								{
									HibernateObject hibernateObject = (HibernateObject) component.getDefaultModelObject();
									permissieConstraint.setScopeObjectId(hibernateObject.getId());
								}
							}
						}
					}
					var valtBinnenOrganisatieTypeScopes = ScreenitSession.get().inScope(constraint);
					if (!valtBinnenOrganisatieTypeScopes)
					{
						fail = constraint;
						break;
					}
					boolean isAny = constraint.required().equals(Required.ANY);
					for (Recht recht : constraint.recht())
					{
						permissieConstraint.setRecht(recht);
						fail = getConstraint(constraint, sm, subject, permissieConstraint);
						if (fail == null && isAny)
						{
							break;
						}
						else if (fail != null && !isAny)
						{
							break;
						}
					}
				}

				if (fail != null)
				{
					break;
				}
			}
		}
		return fail;
	}

	private SecurityConstraint getConstraint(SecurityConstraint constraint, SecurityManager sm, Subject subject, Permission permission)
	{
		SecurityConstraint fail = null;
		switch (constraint.constraint())
		{
		case HasPermission:
			if (!sm.isPermitted(subject.getPrincipals(), permission))
			{
				fail = constraint;
			}
			break;

		case LoggedIn:
			if (subject.getPrincipal() == null)
			{
				fail = constraint;
			}
			break;

		default:
			throw new IllegalStateException("Unknow or unsupported constraint");
		}

		return fail;
	}

	@Override
	public boolean isActionAuthorized(final Component component, final Action action)
	{
		if (action.getName().equals(Component.RENDER))
		{
			Class<? extends Component> clazz = component.getClass();
			SecurityConstraint fail = checkInvalidInstantiation(clazz.getAnnotations(), component);
			if (fail == null)
			{
				fail = checkInvalidInstantiation(clazz.getPackage().getAnnotations(), component);
			}
			return fail == null;
		}
		else if (action instanceof ConstraintAction)
		{
			ConstraintAction constraintAction = (ConstraintAction) action;
			Subject subject = SecurityUtils.getSubject();
			SecurityManager sm = ThreadContext.getSecurityManager();
			return sm.isPermitted(subject.getPrincipals(), constraintAction.getConstraint());
		}
		return true;
	}

	@Override
	public boolean isResourceAuthorized(IResource resource, PageParameters parameters)
	{
		return checkInvalidInstantiation(resource.getClass().getAnnotations(), null) == null;
	}
}
