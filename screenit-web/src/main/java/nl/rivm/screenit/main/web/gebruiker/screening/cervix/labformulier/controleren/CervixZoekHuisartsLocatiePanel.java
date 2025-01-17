package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.form.AjaxInnerFormSubmitBehavior;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierenFilter;
import nl.rivm.screenit.service.cervix.CervixHuisartsLocatieFilter;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.validation.validator.StringValidator;

public abstract class CervixZoekHuisartsLocatiePanel extends GenericPanel<CervixHuisartsLocatieFilter>
{
	protected CervixZoekHuisartsLocatiePanel(String id)
	{
		super(id, new CompoundPropertyModel<>(new CervixHuisartsLocatieFilter()));

		Form<CervixLabformulierenFilter> form = new Form<>("innerForm");
		add(form);

		form.add(new TextField<>("achternaam"));
		ComponentHelper.addTextField(form, "agbcode", false, 8, false).add(StringValidator.minimumLength(5));
		form.add(new TextField<>("locatieNaam"));
		form.add(createAdresZoekvelden());

		form.add(new AjaxInnerFormSubmitBehavior("change")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				target.add(refreshTable());
			}
		});

		refreshTable();
	}

	private WebMarkupContainer createAdresZoekvelden()
	{
		WebMarkupContainer adresZoekVelden = new WebMarkupContainer("adresZoekvelden");
		adresZoekVelden.add(new TextField<String>("straat"));
		adresZoekVelden.add(new PostcodeField("postcode").setAlleenCijfersToegestaan(true));
		adresZoekVelden.add(new TextField<String>("plaats"));
		adresZoekVelden.setVisible(toonAdresVelden());
		return adresZoekVelden;
	}

	protected abstract boolean toonAdresVelden();

	private WebMarkupContainer refreshTable()
	{
		if (filterLeeg())
		{
			EmptyPanel huisartsLocaties = new EmptyPanel("huisartsLocaties");
			huisartsLocaties.setOutputMarkupPlaceholderTag(true);
			addOrReplace(huisartsLocaties);
			return huisartsLocaties;
		}
		else
		{
			List<IColumn<CervixHuisartsLocatie, String>> columns = new ArrayList<>();
			columns.add(new PropertyColumn<CervixHuisartsLocatie, String>(Model.of("Huisarts"), "medewerker.achternaam", "huisarts")
			{
				@Override
				public IModel<String> getDataModel(IModel<CervixHuisartsLocatie> huisartsLocatieModel)
				{
					CervixHuisarts huisarts = new PropertyModel<CervixHuisarts>(huisartsLocatieModel, getPropertyExpression()).getObject();
					return Model.of(NaamUtil.getNaamHuisarts(huisarts));
				}
			});
			columns.add(new PropertyColumn<>(Model.of("AGB-code"), "huisarts.agbcode", "huisarts.agbcode"));
			columns.add(new PropertyColumn<>(Model.of("Locatienaam"), "huisartsLocatie.naam", "naam"));

			if (toonAdresVelden())
			{
				columns.add(new PropertyColumn<CervixHuisartsLocatie, String>(Model.of("Adres"), "woonplaats.naam", "woonplaats.naam")
				{
					@Override
					public IModel<String> getDataModel(IModel<CervixHuisartsLocatie> huisartsLocatieModel)
					{
						CervixHuisartsLocatie huisartsLocatie = huisartsLocatieModel.getObject();
						String adres = huisartsLocatie.getLocatieAdres() != null ? AdresUtil.getVolledigeAdresString(huisartsLocatie.getLocatieAdres()) : "";
						return Model.of(adres);
					}
				});
			}

			CervixHuisartsLocatieProvider huisartsLocatieProvider = new CervixHuisartsLocatieProvider(CervixZoekHuisartsLocatiePanel.this.getModelObject());

			ScreenitDataTable<CervixHuisartsLocatie, String> huisartsLocaties = new ScreenitDataTable<CervixHuisartsLocatie, String>("huisartsLocaties", columns,
				huisartsLocatieProvider, 10, Model.of("huisarts locaties"))
			{
				@Override
				public void onClick(AjaxRequestTarget target, IModel<CervixHuisartsLocatie> huisartsLocatieModel)
				{
					setHuisartsLocatie(target, huisartsLocatieModel.getObject());
				}
			};
			addOrReplace(huisartsLocaties);

			return huisartsLocaties;
		}
	}

	private boolean filterLeeg()
	{
		CervixHuisartsLocatieFilter filter = CervixZoekHuisartsLocatiePanel.this.getModelObject();
		return StringUtils.isBlank(filter.getAchternaam()) && StringUtils.isBlank(filter.getAgbcode()) && StringUtils.isBlank(filter.getLocatieNaam())
			&& StringUtils.isBlank(filter.getPostcode()) && StringUtils.isBlank(filter.getPlaats()) && StringUtils.isBlank(filter.getStraat());
	}

	public abstract void setHuisartsLocatie(AjaxRequestTarget target, CervixHuisartsLocatie huisartsLocatie);
}
