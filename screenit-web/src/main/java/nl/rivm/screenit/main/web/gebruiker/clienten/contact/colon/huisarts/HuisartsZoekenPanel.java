package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.huisarts;

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
import java.util.Iterator;

import nl.rivm.screenit.main.service.impl.EnovationHuisartsDataProviderServiceImpl;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.enums.HuisartsGeslacht;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
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

import static nl.rivm.screenit.model.Huisarts_.ACHTERNAAM;
import static nl.rivm.screenit.model.Huisarts_.ADRES;
import static nl.rivm.screenit.model.Huisarts_.GESLACHT;
import static nl.rivm.screenit.model.Huisarts_.PRAKTIJKNAAM;
import static nl.topicuszorg.organisatie.model.Adres_.PLAATS;

public abstract class HuisartsZoekenPanel extends GenericPanel<EnovationHuisarts>
{
	@SpringBean
	private EnovationHuisartsDataProviderServiceImpl enovationHuisartsDataProviderService;

	private WebMarkupContainer zoekResultatenContainer;

	private final boolean terugNaarZoeken;

	public HuisartsZoekenPanel(String id, boolean terugNaarZoeken)
	{
		super(id, ModelUtil.ccModel(new EnovationHuisarts()));
		this.terugNaarZoeken = terugNaarZoeken;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		var screenitForm = new ScreenitForm<>("zoekForm", getModel())
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

		var zoekenBtn = new IndicatingAjaxSubmitLink("zoeken")
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
		var aantal = enovationHuisartsDataProviderService.size(getModelObject());
		return new Label("aantalResultaten", String.format(getString("label.aantal.gevonden.huisartsen"), aantal));
	}

	private ScreenitDataTable<EnovationHuisarts, String> vervangResultaten()
	{
		var columns = new ArrayList<IColumn<EnovationHuisarts, String>>();
		columns.add(new PropertyColumn<>(Model.of("Naam"), ACHTERNAAM, ACHTERNAAM)
		{
			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public IModel<Object> getDataModel(IModel<EnovationHuisarts> rowModel)
			{
				var huisarts = rowModel.getObject();
				var naam = NaamUtil.getNaamHuisarts(huisarts);
				return new Model(naam);
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Type"), GESLACHT, GESLACHT)
		{
			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public IModel<Object> getDataModel(IModel<EnovationHuisarts> rowModel)
			{
				var huisarts = rowModel.getObject();
				var type = "";
				if (huisarts.getGeslacht() != null)
				{
					type = HuisartsGeslacht.ORGANISATIE.equals(huisarts.getGeslacht()) ? "Huisartsenpraktijk" : "Huisarts";
				}
				return new Model(type);
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Adres"), ADRES + "." + PLAATS, ADRES + "." + PLAATS)
		{
			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public IModel<Object> getDataModel(IModel<EnovationHuisarts> rowModel)
			{
				var huisarts = rowModel.getObject();
				var adres = "";
				if (huisarts.getAdres() != null)
				{
					adres = AdresUtil.getVolledigeAdresString(huisarts.getAdres());
				}
				return new Model(adres);
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Praktijknaam"), PRAKTIJKNAAM, PRAKTIJKNAAM));

		var tabel = new ScreenitDataTable<>("zoekResultaten", columns, new SortableDataProvider<EnovationHuisarts, String>()
		{
			@Override
			public Iterator<? extends EnovationHuisarts> iterator(long first, long count)
			{
				if (getSort() == null)
				{
					setSort(ACHTERNAAM, SortOrder.ASCENDING);
				}

				return enovationHuisartsDataProviderService.findPage(first, count, getModelObject(), getSort()).iterator();
			}

			@Override
			public long size()
			{
				return enovationHuisartsDataProviderService.size(getModelObject());
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
				var ha = huisartsModel.getObject();
				onHuisartsGekozen(target, ha);
			}
		};
		return tabel;
	}

	protected abstract void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts);
}
