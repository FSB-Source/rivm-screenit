package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.ParameterisatiePropertyModel;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;

public class TechnischBeheerOpenIDConnectPanel extends BaseTechnischBeheerParametersPanel
{
	public TechnischBeheerOpenIDConnectPanel(String id, IModel<Parameterisatie> model)
	{
		super(id, new ParameterisatiePropertyModel<>(model));
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		Form<Parameterisatie> form = new Form<>("form");

		form.add(new TextField<>("internalOpenidConnectIdpKeystorepassword", String.class).setRequired(true));
		form.add(new TextField<>("internalOpenidConnectIdpKeystore", String.class).setRequired(true));
		form.add(new TextField<>("internalOpenidConnectIdpIssuer", String.class).setRequired(true));
		form.add(new TextField<>("internalOpenidConnectIdpKeycloakOrigin", String.class).setRequired(true));
		form.add(new TextField<>("internalOpenidConnectIdpClientId", String.class).setRequired(true));
		form.add(new TextField<>("internalOpenidConnectIdpFlowAlias", String.class).setRequired(true));
		form.add(new TextField<>("internalOpenidConnectIdpExpiration", Integer.class).setRequired(true));

		return form;
	}

	@Override
	protected Component createAndGetOpslaanLink()
	{
		return new IndicatingAjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				info("Parameters zijn opgeslagen");
				opslaan(target);
			}
		};
	}
}
