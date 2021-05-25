package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.huisarts;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonHuisartsWijzigenPanel;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.HuisartsGeslacht;
import nl.rivm.screenit.service.EnovationHuisartsService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.annot.objects.Transient;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
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

public abstract class HuisartsZoekenDialogPanel extends GenericPanel<ColonScreeningRonde>
{

	private static final long serialVersionUID = 1L;

	private WebMarkupContainer zoekResultatenContainer;

	@SpringBean
	private EnovationHuisartsService enovationHuisartsService;

	private ColonHuisartsWijzigenPanel huisartsWijzigenPanel;

	@Transient
	private IModel<EnovationHuisarts> zoekModel;

	private BootstrapDialog dialog;

	public HuisartsZoekenDialogPanel(String id, IModel<ColonScreeningRonde> colonScreeningRonde, IModel<EnovationHuisarts> zoekModel, BootstrapDialog dialog,
		boolean terugNaarZoeken, ColonHuisartsWijzigenPanel huisartsWijzigenPanel)
	{
		super(id, colonScreeningRonde);
		setZoekModel(zoekModel);

		setDialog(dialog);
		setHuisartsWijzigenPanel(huisartsWijzigenPanel);

		ScreenitForm<EnovationHuisarts> screenitForm = new ScreenitForm<EnovationHuisarts>("zoekForm", getZoekModel())
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

		add(new AjaxLink<ColonScreeningRonde>("huisartsOnbekend", getModel())
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);

				getDialog().openWith(target, new OnbekendeHuisartsMakenDialogPanel(IDialog.CONTENT_ID, getModel(), getHuisartsWijzigenPanel(), getDialog())
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void close(AjaxRequestTarget target)
					{
						getDialog().close(target);
					}
				});
			}
		});
		add(new AjaxLink<Void>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(getZoekModel());
	}

	private Label aantalResultaten()
	{
		long aantal = enovationHuisartsService.telHuisartsen(getZoekModel().getObject());
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

		ScreenitDataTable<EnovationHuisarts, String> tabel = new ScreenitDataTable<EnovationHuisarts, String>("zoekResultaten", columns,
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

					return enovationHuisartsService.zoekHuisartsen(getZoekModel().getObject(), getSort().getProperty(), getSort().isAscending(), (int) first, (int) count)
						.iterator();
				}

				@Override
				public long size()
				{
					return enovationHuisartsService.telHuisartsen(getZoekModel().getObject());
				}

				@Override
				public IModel<EnovationHuisarts> model(EnovationHuisarts object)
				{
					return ModelUtil.sModel(object);
				}
			}, Model.of("huisarts(en)"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<EnovationHuisarts> huisartsModel)
			{
				EnovationHuisarts ha = huisartsModel.getObject();
				ColonScreeningRonde laatsteRonde = getModelObject();
				laatsteRonde.setColonHuisarts(ha);
				if (laatsteRonde.getOnbekendeHuisarts() != null)
				{
					laatsteRonde.setOnbekendeHuisarts(null);
				}
				getHuisartsWijzigenPanel().verversHuisarts(target);
				getDialog().close(target);
			}
		};
		return tabel;
	}

	public IModel<EnovationHuisarts> getZoekModel()
	{
		return zoekModel;
	}

	public void setZoekModel(IModel<EnovationHuisarts> zoekModel)
	{
		this.zoekModel = zoekModel;
	}

	protected abstract void close(AjaxRequestTarget target);

	public BootstrapDialog getDialog()
	{
		return dialog;
	}

	public void setDialog(BootstrapDialog dialog)
	{
		this.dialog = dialog;
	}

	private ColonHuisartsWijzigenPanel getHuisartsWijzigenPanel()
	{
		return huisartsWijzigenPanel;
	}

	private void setHuisartsWijzigenPanel(ColonHuisartsWijzigenPanel huisartsWijzigenPanel)
	{
		this.huisartsWijzigenPanel = huisartsWijzigenPanel;
	}
}
