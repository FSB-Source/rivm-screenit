package nl.rivm.screenit.main.web.gebruiker.screening.cervix.hl7v2berichten;

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

import java.time.Duration;
import java.util.ArrayList;
import java.util.Iterator;

import nl.rivm.screenit.main.service.cervix.CervixHL7v2FoutberichtService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.PollingAbstractAjaxTimerBehavior;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.NotClickablePropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.ScreenitDateTimePropertyColumn;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.cervix.berichten.CervixFoutHL7v2Bericht;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.StringResourceModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import com.google.common.primitives.Ints;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.CERVIX })
public class CervixVerwerkHL7v2FoutBerichtenPage extends GebruikerBasePage
{

	@SpringBean
	private CervixHL7v2FoutberichtService hl7v2FoutberichtService;

	@SpringBean
	private HibernateService hibernateService;

	private Component berichtenTabel;

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		addOrReplaceTable(null);
		createTimer();
		add(new IndicatingAjaxLink<Void>("refresh")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				target.add(berichtenTabel);
			}

		});
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.CERVIX;
	}

	private void addOrReplaceTable(AjaxRequestTarget target)
	{
		var columns = new ArrayList<IColumn<CervixFoutHL7v2Bericht, String>>();
		columns.add(new ScreenitDateTimePropertyColumn<>(Model.of("Datum/tijd response"), "responseMoment", "responseMoment"));
		columns.add(new PropertyColumn<>(Model.of("Laboratorium"), "laboratorium.naam", "laboratorium.naam"));
		columns.add(new ClientColumn<>("persoon.achternaam", "client"));
		columns.add(new PropertyColumn<>(Model.of("Request"), "request"));
		columns.add(new PropertyColumn<>(Model.of("Response"), "response"));

		addBerichtOpnieuwAanbiedenKolom(columns);
		addBerichtVerwijderenKolom(columns);

		var magClientDossierInzien = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_GEGEVENS, Actie.INZIEN);

		Component newScreenitDataTable = new ScreenitDataTable<>("tabel", columns, new Hl7FoutBerichtenProvider(), 10,
			Model.of("fout(e) bericht(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<CervixFoutHL7v2Bericht> model)
			{
				super.onClick(target, model);
				if (magClientDossierInzien)
				{
					var client = model.getObject().getClient();
					if (client != null && !GbaStatus.BEZWAAR.equals(client.getGbaStatus()) && !GbaStatus.AFGEVOERD.equals(client.getGbaStatus()))
					{
						setResponsePage(new ClientInzienPage(ModelUtil.sModel(client)));
					}
				}
			}
		};
		newScreenitDataTable.setOutputMarkupId(true);
		if (berichtenTabel != null)
		{
			berichtenTabel.replaceWith(newScreenitDataTable);
		}
		else
		{
			add(newScreenitDataTable);
		}
		berichtenTabel = newScreenitDataTable;

		if (target != null)
		{
			target.add(berichtenTabel);
		}
	}

	private void addBerichtOpnieuwAanbiedenKolom(ArrayList<IColumn<CervixFoutHL7v2Bericht, String>> columns)
	{
		var magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN, Actie.AANPASSEN);
		if (magAanpassen)
		{
			columns.add(new NotClickablePropertyColumn<>(Model.of("Opnieuw aanbieden"), "")
			{
				@Override
				public void populateItem(Item<ICellPopulator<CervixFoutHL7v2Bericht>> cellItem, String componentId, final IModel<CervixFoutHL7v2Bericht> rowModel)
				{
					cellItem.add(new AjaxImageCellPanel<>(componentId, rowModel, "icon-refresh")
					{
						@Override
						protected void onClick(AjaxRequestTarget target)
						{
							berichtOpnieuwAanbieden(getModel(), target);
						}
					});
				}

				@Override
				public String getCssClass()
				{
					return "status";
				}

			});
		}
	}

	private void addBerichtVerwijderenKolom(ArrayList<IColumn<CervixFoutHL7v2Bericht, String>> columns)
	{
		var magVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN, Actie.VERWIJDEREN);
		if (magVerwijderen)
		{
			columns.add(new NotClickablePropertyColumn<>(Model.of("Verwijderen"), "")
			{

				@Override
				public void populateItem(Item<ICellPopulator<CervixFoutHL7v2Bericht>> cellItem, String componentId, final IModel<CervixFoutHL7v2Bericht> rowModel)
				{
					final var imageCellPanel = new AjaxImageCellPanel<>(componentId, rowModel, "icon-trash")
					{

						@Override
						protected void onClick(AjaxRequestTarget target)
						{
							getDialog().openWith(target,
								new ConfirmPanel(IDialog.CONTENT_ID,
									new StringResourceModel("ongeldigBerichtVerwijderen.header", CervixVerwerkHL7v2FoutBerichtenPage.this),
									new StringResourceModel("ongeldigBerichtVerwijderen.content", CervixVerwerkHL7v2FoutBerichtenPage.this),
									new DefaultConfirmCallback()
									{
										@Override
										public void onYesClick(AjaxRequestTarget target)
										{
											verwijderMelding(getModel(), target);
										}

									}, getDialog()));
						}
					};
					cellItem.add(imageCellPanel);
				}

				@Override
				public String getCssClass()
				{
					return "status";
				}

			});
		}
	}

	private class Hl7FoutBerichtenProvider extends SortableDataProvider<CervixFoutHL7v2Bericht, String>
	{

		@Override
		public Iterator<? extends CervixFoutHL7v2Bericht> iterator(long first, long count)
		{
			return hibernateService.loadAll(CervixFoutHL7v2Bericht.class).subList(Ints.checkedCast(first), Ints.checkedCast(first + count)).iterator();
		}

		@Override
		public long size()
		{
			return hibernateService.loadAll(CervixFoutHL7v2Bericht.class).size();
		}

		@Override
		public IModel<CervixFoutHL7v2Bericht> model(CervixFoutHL7v2Bericht object)
		{
			return ModelUtil.sModel(object);
		}
	}

	private void createTimer()
	{
		var timer = new PollingAbstractAjaxTimerBehavior(Duration.ofSeconds(5))
		{
			@Override
			protected void onTimer(AjaxRequestTarget target)
			{
				super.onTimer(target);
				target.add(berichtenTabel);
			}
		};
		add(timer);
	}

	private void berichtOpnieuwAanbieden(IModel<CervixFoutHL7v2Bericht> model, AjaxRequestTarget target)
	{
		hl7v2FoutberichtService.berichtOpnieuwAanbieden(model.getObject());
		ScreenitSession.get().info(getString("bericht.opnieuw.aangeboden"));
		addOrReplaceTable(target);
	}

	private void verwijderMelding(IModel<CervixFoutHL7v2Bericht> model, AjaxRequestTarget target)
	{
		hl7v2FoutberichtService.verwijderBericht(model.getObject());
		ScreenitSession.get().warn(getString("bericht.verwijderd"));
		addOrReplaceTable(target);
	}

}
