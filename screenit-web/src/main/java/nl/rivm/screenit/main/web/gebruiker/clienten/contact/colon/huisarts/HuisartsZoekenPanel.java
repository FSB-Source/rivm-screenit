package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.huisarts;

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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.enums.HuisartsGeslacht;
import nl.rivm.screenit.service.EnovationHuisartsService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class HuisartsZoekenPanel extends GenericPanel<EnovationHuisarts>
{
	@SpringBean
	private EnovationHuisartsService enovationHuisartsService;

	private WebMarkupContainer zoekResultatenContainer;

	private boolean terugNaarZoeken;

	public HuisartsZoekenPanel(String id, boolean terugNaarZoeken)
	{
		super(id, ModelUtil.ccModel(new EnovationHuisarts()));
		this.terugNaarZoeken = terugNaarZoeken;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		ScreenitForm<EnovationHuisarts> screenitForm = new ScreenitForm<>("zoekForm", getModel())
		{
			@Override
			public boolean isRootForm()
			{
				return true;
			}
		};
		screenitForm.setOutputMarkupId(true);
		screenitForm.add(new TextField<String>("achternaam"));
		screenitForm.add(new PostcodeField("adres.postcode").setAlleenCijfersToegestaan(true));
		screenitForm.add(new TextField<String>("adres.plaats"));
		screenitForm.add(new TextField<String>("adres.straat"));

		AjaxSubmitLink zoekenBtn = new IndicatingAjaxSubmitLink("zoeken")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				zoekResultatenContainer.setVisible(true);
				zoekResultatenContainer.addOrReplace(aantalResultaten());
				zoekResultatenContainer.addOrReplace(vervangResultaten());
				target.add(zoekResultatenContainer);
			}
		};
		screenitForm.setDefaultButton(zoekenBtn);
		screenitForm.add(zoekenBtn);
		zoekResultatenContainer = new WebMarkupContainer("zoekResultatenContainer");
		zoekResultatenContainer.setOutputMarkupPlaceholderTag(true);
		if (!terugNaarZoeken)
		{
			zoekResultatenContainer.setVisible(false);
			zoekResultatenContainer.add(new WebMarkupContainer("aantalResultaten"));
			zoekResultatenContainer.add(new WebMarkupContainer("zoekResultaten"));
		}
		else
		{
			zoekResultatenContainer.setVisible(true);
			zoekResultatenContainer.addOrReplace(aantalResultaten());
			zoekResultatenContainer.addOrReplace(vervangResultaten());
		}
		add(zoekResultatenContainer);
		add(screenitForm);
	}

	private Label aantalResultaten()
	{
		long aantal = enovationHuisartsService.telHuisartsen(getModelObject());
		return new Label("aantalResultaten", String.format(getString("label.aantal.gevonden.huisartsen"), Long.toString(aantal)));
	}

	private ScreenitDataTable<EnovationHuisarts, String> vervangResultaten()
	{
		List<IColumn<EnovationHuisarts, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Naam"), "achternaam", "achternaam")
		{
			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public IModel<Object> getDataModel(IModel<EnovationHuisarts> rowModel)
			{
				EnovationHuisarts huisarts = rowModel.getObject();
				String naam = NaamUtil.getNaamHuisarts(huisarts);
				return new Model(naam);
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Type"), "geslacht", "geslacht")
		{
			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public IModel<Object> getDataModel(IModel<EnovationHuisarts> rowModel)
			{
				EnovationHuisarts huisarts = rowModel.getObject();
				String type = "";
				if (huisarts.getGeslacht() != null)
				{
					type = HuisartsGeslacht.ORGANISATIE.equals(huisarts.getGeslacht()) ? "Huisartsenpraktijk" : "Huisarts";
				}
				return new Model(type);
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Adres"), "adres.plaats", "adres.plaats")
		{
			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public IModel<Object> getDataModel(IModel<EnovationHuisarts> rowModel)
			{
				EnovationHuisarts huisarts = rowModel.getObject();
				String adres = "";
				if (huisarts.getAdres() != null)
				{
					adres = AdresUtil.getVolledigeAdresString(huisarts.getAdres());
				}
				return new Model(adres);
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Praktijknaam"), "praktijknaam", "praktijknaam"));

		ScreenitDataTable<EnovationHuisarts, String> tabel = new ScreenitDataTable<>("zoekResultaten", columns, new SortableDataProvider<EnovationHuisarts, String>()
		{
			@Override
			public Iterator<? extends EnovationHuisarts> iterator(long first, long count)
			{
				if (getSort() == null)
				{
					setSort("achternaam", SortOrder.ASCENDING);
				}

				return enovationHuisartsService.zoekHuisartsen(getModelObject(), getSort().getProperty(), getSort().isAscending(), (int) first, (int) count).iterator();
			}

			@Override
			public long size()
			{
				return enovationHuisartsService.telHuisartsen(getModelObject());
			}

			@Override
			public IModel<EnovationHuisarts> model(EnovationHuisarts object)
			{
				return ModelUtil.sModel(object);
			}
		}, Model.of("huisarts(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<EnovationHuisarts> huisartsModel)
			{
				EnovationHuisarts ha = huisartsModel.getObject();
				onHuisartsGekozen(target, ha);
			}
		};
		return tabel;
	}

	protected abstract void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts);
}
