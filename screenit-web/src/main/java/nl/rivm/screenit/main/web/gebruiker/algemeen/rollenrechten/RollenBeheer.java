package nl.rivm.screenit.main.web.gebruiker.algemeen.rollenrechten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.Panel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_ROLLEN_BEHEREN, bevolkingsonderzoekScopes = {
	Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class RollenBeheer extends AlgemeenPage
{

	static final String CONTENT_ID = "content";

	private static final long serialVersionUID = 1L;

	private Panel content;

	public RollenBeheer()
	{
		addOrReplaceContentWith(new RollenOverzichtPanel("content"));
	}

	public void addOrReplaceContentWith(Panel panel)
	{
		panel.setOutputMarkupId(true);
		AjaxRequestTarget target = getRequestCycle().find(AjaxRequestTarget.class).orElse(null);
		if (target != null && content != null)
		{
			content.replaceWith(panel);
			content = panel;
			target.add(content);
		}
		else
		{
			content = panel;
			add(content);
		}
	}
}
