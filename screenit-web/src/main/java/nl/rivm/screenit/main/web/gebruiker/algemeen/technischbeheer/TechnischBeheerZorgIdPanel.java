package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

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

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.ParameterisatiePropertyModel;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.validation.validator.RangeValidator;

public class TechnischBeheerZorgIdPanel extends BaseTechnischBeheerParametersPanel
{
	public TechnischBeheerZorgIdPanel(String id, IModel<Parameterisatie> model)
	{
		super(id, new ParameterisatiePropertyModel<>(model));
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		Form<Parameterisatie> form = new Form<>("form");

		form.add(new TextField<>("internalZorgidCallbackurl", String.class).setRequired(true));
		form.add(new TextField<>("internalZorgidServerurl", String.class).setRequired(true));
		form.add(new TextField<>("internalZorgidClientreadtimeout", Integer.class).add(RangeValidator.minimum(1)).setRequired(true));
		form.add(new TextField<>("internalZorgidTransactionclientreadtimeout", Integer.class).add(RangeValidator.minimum(1)).setRequired(true));
		form.add(new TextField<>("internalZorgidClientconnecttimeout", Integer.class).add(RangeValidator.minimum(1)).setRequired(true));
		form.add(new TextField<>("internalZorgidClientmaxconnections", Integer.class).add(RangeValidator.minimum(1)).setRequired(true));
		form.add(new TextField<>("internalZorgidWebservicekeystore", String.class).setRequired(true));
		form.add(new TextField<>("internalZorgidWebservicekeystorepassword", String.class).setRequired(true));
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
