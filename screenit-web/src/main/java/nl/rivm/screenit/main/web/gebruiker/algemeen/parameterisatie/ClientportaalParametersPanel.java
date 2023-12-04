package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Arrays;

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.validation.validator.RangeValidator;
import org.apache.wicket.validation.validator.StringValidator;

public class ClientportaalParametersPanel extends BasePrimaireParametersPanel
{
	public ClientportaalParametersPanel(String id, IModel<Parameterisatie> model)
	{
		super(id, model);
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		Form<Parameterisatie> form = new Form<>("form");

		ListView<Bevolkingsonderzoek> bvos = new ListView<Bevolkingsonderzoek>("bvos", Arrays.asList(Bevolkingsonderzoek.values()))
		{

			@Override
			protected void populateItem(ListItem<Bevolkingsonderzoek> item)
			{
				item.add(new EnumLabel<Bevolkingsonderzoek>("naam", item.getModelObject()));
				String bvoLowerCase = item.getModelObject().name().toLowerCase();
				item.add(
					new CheckBox("toonVervangendeTekst",
						new ParameterisatieClientportaalPropertyModel<Parameterisatie, Boolean>(ClientportaalParametersPanel.this.getModelObject(),
							bvoLowerCase + "ClientportaalToonVervangendeTekst")));
				TextArea<String> vervangendeTekst = new TextArea<>("vervangendeTekst",
					new ParameterisatieClientportaalPropertyModel<Parameterisatie, String>(ClientportaalParametersPanel.this.getModelObject(),
						bvoLowerCase + "ClientportaalVervangendeTekst"));
				vervangendeTekst.setOutputMarkupId(true);
				vervangendeTekst.add(new StringValidator(1, 3999));
				item.add(vervangendeTekst);
				TextArea<String> tijdelijkeMelding = new TextArea<>("tijdelijkeMelding",
						new ParameterisatieClientportaalPropertyModel<Parameterisatie, String>(ClientportaalParametersPanel.this.getModelObject(),
								bvoLowerCase + "ClientportaalTijdelijkeMelding"));
				tijdelijkeMelding.setOutputMarkupId(true);
				tijdelijkeMelding.add(new StringValidator(1, 3999));
				item.add(tijdelijkeMelding);

				item.add(new TextField<>("mammaOnderzoekscapaciteitNietBeschikbaarBinnenWerkdagen", Integer.class).add(RangeValidator.minimum(1))
					.setVisible(item.getModelObject().equals(Bevolkingsonderzoek.MAMMA)));
			}

		};
		form.add(bvos);

		return form;
	}

	@Override
	protected Component createAndGetOpslaanLink()
	{
		return new AjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
			opslaan(target, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA, Bevolkingsonderzoek.COLON);
				info("Clientportaal parameters zijn opgeslagen");

			}
		};
	}
}
