package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

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
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.Model;

public class WachtwoordConfiguratiePanel extends BasePrimaireParametersPanel
{

	private static final long serialVersionUID = 1L;

	public WachtwoordConfiguratiePanel(String id, Model<Parameterisatie> model)
	{
		super(id, model);
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		var form = new Form<Parameterisatie>("form");
		form.add(new CheckBox("wachtwoordaanvragen"));
		addTextAreaField(form, "wachtwoordemail");
		addTextAreaField(form, "uziemail");
		addTextAreaField(form, "wachtwoordverlooptemail");
		ComponentHelper.addTextField(form, "wachtwoordemailsubject", true, 4000, false);
		ComponentHelper.addTextField(form, "uziemailsubject", true, 4000, false);
		ComponentHelper.addTextField(form, "wachtwoordverlooptemailsubject", true, 4000, false);
		ComponentHelper.addTextField(form, "dagenWachtwoordGeldig", true, 20, Integer.class, false);
		ComponentHelper.addTextField(form, "maximumFoutieveAanmeldpogingen", true, 20, Integer.class, false);
		ComponentHelper.addTextField(form, "foutieveAanmeldpogingenTimeout", true, 20, Integer.class, false);
		ComponentHelper.addTextField(form, "wachtwoordVerlooptHerinneringsTermijn", true, 40, Integer.class, false);

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
				opslaan(target, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA, Bevolkingsonderzoek.CERVIX);
				info("Wachtwoord configuratie is opgeslagen.");
			}
		};
	}
}
