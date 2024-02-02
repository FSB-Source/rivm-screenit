package nl.rivm.screenit.main.web.gebruiker.screening.colon.verslagen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.BerichtenZoekFilter;
import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.ColonScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, checkScope = false, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_VERSLAGEN, bevolkingsonderzoekScopes = {
	Bevolkingsonderzoek.COLON })
public class VerwerkteBerichtenOverzichtPage extends ColonScreeningBasePage
{
	private static final Logger LOG = LoggerFactory.getLogger(VerwerkteBerichtenOverzichtPage.class);

	private final BootstrapDialog dialog;

	@SpringBean
	private VerslagService verslagService;

	private Component berichtenTabel;

	private final IModel<BerichtZoekFilter> berichtZoekFilter;

	public VerwerkteBerichtenOverzichtPage()
	{
		List<Bevolkingsonderzoek> onderzoeken = ScreenitSession.get().getOnderzoeken();
		BerichtenZoekFilter berichtenZoekFilter = new BerichtenZoekFilter();
		if (onderzoeken.contains(Bevolkingsonderzoek.COLON))
		{
			berichtenZoekFilter.setMldBerichten(true);
			berichtenZoekFilter.setPaLabBerichten(true);
		}
		else
		{
			berichtenZoekFilter.setMldBerichten(false);
			berichtenZoekFilter.setPaLabBerichten(false);
		}

		berichtenZoekFilter.setCytologieBerichten(false);

		berichtZoekFilter = new Model<>(berichtenZoekFilter);

		add(new FilterForm("form", berichtZoekFilter));

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		addOrReplaceTable(null);

	}

	private void addOrReplaceTable(AjaxRequestTarget target)
	{

		List<IColumn<OntvangenCdaBericht, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<OntvangenCdaBericht, String>(Model.of("Datum/tijd ontvangst"), "ontvangen", "ontvangen"));
		columns.add(new PropertyColumn<OntvangenCdaBericht, String>(Model.of("DocumentID"), "berichtId", "berichtId"));
		columns.add(new PropertyColumn<OntvangenCdaBericht, String>(Model.of("SetID"), "setId", "setId"));
		columns.add(new PropertyColumn<OntvangenCdaBericht, String>(Model.of("Versie"), "versie", "versie"));
		columns.add(new PropertyColumn<OntvangenCdaBericht, String>(Model.of("Projectversie"), "projectVersion", "projectVersion"));
		columns.add(new PropertyColumn<OntvangenCdaBericht, String>(Model.of("Type"), "berichtType", "berichtType"));
		columns.add(new AbstractColumn<OntvangenCdaBericht, String>(Model.of("Bekijk bericht"))
		{

			@Override
			public void populateItem(Item<ICellPopulator<OntvangenCdaBericht>> cellItem, String componentId, IModel<OntvangenCdaBericht> rowModel)
			{
				cellItem.add(new AjaxImageCellPanel<OntvangenCdaBericht>(componentId, rowModel, "icon-info-sign")
				{
					@Override
					protected void onClick(AjaxRequestTarget target)
					{
						dialog.openWith(target, new VerwerktBerichtInzienPanel(IDialog.CONTENT_ID, getModel())
						{

							@Override
							protected void opnieuwAanbieden(IModel<OntvangenCdaBericht> model, AjaxRequestTarget target)
							{
								dialog.close(target);
								berichtOpnieuwAanbieden(model.getObject(), target);
							}

						});
					}
				});

			}

			@Override
			public String getCssClass()
			{
				return "status";
			}

		});
		final boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_VERSLAGEN, Actie.AANPASSEN);
		if (magAanpassen)
		{
			columns.add(new AbstractColumn<OntvangenCdaBericht, String>(Model.of("Opnieuw aanbieden"))
			{

				@Override
				public void populateItem(Item<ICellPopulator<OntvangenCdaBericht>> cellItem, String componentId, final IModel<OntvangenCdaBericht> rowModel)
				{
					cellItem.add(new AjaxImageCellPanel<OntvangenCdaBericht>(componentId, rowModel, "icon-refresh")
					{
						@Override
						protected void onClick(AjaxRequestTarget target)
						{
							berichtOpnieuwAanbieden(getModelObject(), target);
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

		Component newScreenitDataTable = new ScreenitDataTable<OntvangenCdaBericht, String>("tabel", columns, new VerwerkteBerichtenOverzichtDataProvider(berichtZoekFilter), 10,
			Model.of("bericht(en)"));
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

	private class FilterForm extends Form<BerichtZoekFilter>
	{
		public FilterForm(String id, IModel<BerichtZoekFilter> model)
		{
			super(id, new CompoundPropertyModel<>(model));

			List<Bevolkingsonderzoek> onderzoeken = ScreenitSession.get().getOnderzoeken();
			boolean colon = onderzoeken.contains(Bevolkingsonderzoek.COLON);
			boolean cervix = onderzoeken.contains(Bevolkingsonderzoek.CERVIX);

			CheckBox mldBerichten = new CheckBox("mldBerichten");
			CheckBox paLabBerichten = new CheckBox("paLabBerichten");
			CheckBox cytologieBerichten = new CheckBox("cytologieBerichten");
			mldBerichten.setVisible(colon);
			paLabBerichten.setVisible(colon);

			cytologieBerichten.setVisible(false);
			add(mldBerichten);
			add(paLabBerichten);
			add(cytologieBerichten);

			add(new TextField<String>("text"));
			add(new IndicatingAjaxButton("filter", this)
			{
				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					addOrReplaceTable(target);
				}
			});

			add(new IndicatingAjaxButton("allesOpnieuwVerwerken", this)
			{
				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					verslagService.herverwerkAlleBerichten(ModelUtil.nullSafeGet(berichtZoekFilter));
					info("Alle gefilterde berichten worden opnieuw aangeboden");
				}
			}.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_VERSLAGEN, Actie.AANPASSEN)));
		}
	}

	private void berichtOpnieuwAanbieden(OntvangenCdaBericht ontvangenCdaBericht, AjaxRequestTarget target)
	{
		verslagService.berichtOpnieuwVerwerken(ontvangenCdaBericht);
		LOG.info("Bericht met ID " + ontvangenCdaBericht.getId() + " wordt opnieuw aangeboden");
		if (target != null)
		{
			info("Ontvangen bericht is opnieuw aangeboden ter verwerking door de batch applicatie.");
			addOrReplaceTable(target);
		}
	}

}
