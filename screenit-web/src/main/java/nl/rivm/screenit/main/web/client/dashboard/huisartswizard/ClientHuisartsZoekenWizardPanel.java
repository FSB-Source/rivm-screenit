package nl.rivm.screenit.main.web.client.dashboard.huisartswizard;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.BaseHuisartsModel;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.main.web.component.table.ClientPortaalDataTable;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.enums.HuisartsGeslacht;
import nl.rivm.screenit.service.EnovationHuisartsService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class ClientHuisartsZoekenWizardPanel extends GenericPanel<EnovationHuisarts>
{
	private static final long serialVersionUID = 1L;

	private WebMarkupContainer zoekResultatenContainer;

	@SpringBean
	private EnovationHuisartsService enovationHuisartsService;

	public ClientHuisartsZoekenWizardPanel(String id, IModel<EnovationHuisarts> zoekModel, boolean toonDirectZoekResultaten)
	{
		super(id, zoekModel);
		boolean magHuisartsVerwijderen = magHuisartsVerwijderen();
		add(new WebMarkupContainer("huisartsVerwijderenTekst").setVisible(magHuisartsVerwijderen));
		add(getHuisartsInfoComponent("huisartsInfo"));
		AjaxLink<Void> verwijderHuisartsBtn = new IndicatingAjaxLink<Void>("huisartsVerwijderen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ScreenitSession.get().success(getString("message.huisarts.verwijderd"));
				onVerwijderHuidigeHuisarts(target);
			}
		};
		verwijderHuisartsBtn.setVisible(magHuisartsVerwijderen);
		add(verwijderHuisartsBtn);

		ScreenitForm<EnovationHuisarts> screenitForm = new ScreenitForm<>("zoekForm");
		screenitForm.add(new TextField<String>("achternaam"));
		screenitForm.add(new TextField<String>("adres.plaats"));
		screenitForm.add(new PostcodeField("adres.postcode").setAlleenCijfersToegestaan(true));
		screenitForm.add(new TextField<String>("adres.straat"));

		AjaxSubmitLink zoeken = new AjaxSubmitLink("zoeken")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				zoekResultatenContainer.setVisible(true);
				zoekResultatenContainer.addOrReplace(aantalResultaten());
				zoekResultatenContainer.addOrReplace(vervangResultaten());
				target.add(zoekResultatenContainer);
			}
		};
		screenitForm.add(zoeken);
		screenitForm.setDefaultButton(zoeken);
		screenitForm.add(new AjaxLink<Void>("annuleren")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onStop(target);
			}
		});

		zoekResultatenContainer = new WebMarkupContainer("zoekResultatenContainer");
		zoekResultatenContainer.setOutputMarkupPlaceholderTag(true);
		if (!toonDirectZoekResultaten)
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

	protected boolean magHuisartsVerwijderen()
	{
		return true;
	}

	protected WebMarkupContainer getHuisartsInfoComponent(String id)
	{
		return new ClientHuisartsInfoPanel(id, getHuisartsModel());
	}

	private Label aantalResultaten()
	{
		long aantal = enovationHuisartsService.telHuisartsen(getModel().getObject());
		return new Label("aantalResultaten", String.format(getString("label.aantal.gevonden.huisartsen", null), Long.toString(aantal)));
	}

	private ScreenitDataTable<EnovationHuisarts, String> vervangResultaten()
	{
		List<IColumn<EnovationHuisarts, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<EnovationHuisarts, String>(Model.of("Naam"), "achternaam", "achternaam")
		{
			private static final long serialVersionUID = 1L;

			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public IModel<Object> getDataModel(IModel<EnovationHuisarts> rowModel)
			{
				EnovationHuisarts huisarts = rowModel.getObject();
				String naam = NaamUtil.getNaamHuisarts(huisarts);
				return new Model(naam);
			}
		});
		columns.add(new PropertyColumn<EnovationHuisarts, String>(Model.of("Type"), "geslacht", "geslacht")
		{
			private static final long serialVersionUID = 1L;

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
		columns.add(new PropertyColumn<EnovationHuisarts, String>(Model.of("Adres"), "adres.plaats", "adres.plaats")
		{
			private static final long serialVersionUID = 1L;

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

		columns.add(new PropertyColumn<EnovationHuisarts, String>(Model.of("Praktijknaam"), "praktijknaam", "praktijknaam"));

		ScreenitDataTable<EnovationHuisarts, String> tabel = new ClientPortaalDataTable<EnovationHuisarts, String>("zoekResultaten", columns,
			new SortableDataProvider<EnovationHuisarts, String>()
			{
				private static final long serialVersionUID = 1L;

				@Override
				public Iterator<? extends EnovationHuisarts> iterator(long first, long count)
				{
					if (getSort() == null)
					{
						setSort("achternaam", SortOrder.ASCENDING);
					}

					return enovationHuisartsService.zoekHuisartsen(getModel().getObject(), getSort().getProperty(), getSort().isAscending(), (int) first, (int) count)
						.iterator();
				}

				@Override
				public long size()
				{
					return enovationHuisartsService.telHuisartsen(getModel().getObject());
				}

				@Override
				public IModel<EnovationHuisarts> model(EnovationHuisarts object)
				{
					return ModelUtil.sModel(object);
				}

			}, 10, Model.of("huisarts(en)"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<EnovationHuisarts> huisartsModel)
			{
				EnovationHuisarts ha = huisartsModel.getObject();
				onHuisartsGekozen(target, ha);
			}
		};

		return tabel;
	}

	protected abstract void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts ha);

	protected abstract void onVerwijderHuidigeHuisarts(AjaxRequestTarget target);

	protected abstract void onStop(AjaxRequestTarget target);

	protected abstract BaseHuisartsModel<?> getHuisartsModel();

}
