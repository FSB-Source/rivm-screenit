package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole.reeks;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.SKMLExternSchemaService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxLinkPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.rivm.screenit.model.colon.SKMLExternSchema_;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_BEHEER_SCHEMA_EXTERNE_CONTROLE,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class SKMLExterneControleSchemaPage extends KwaliteitscontroleBasePage
{
	private IModel<SKMLExternSchema> zoekModel;

	@SpringBean
	private SKMLExternSchemaService schemaService;

	@SpringBean
	private HibernateService hibernateService;

	private ScreenitDataTable<SKMLExternSchema, String> overzicht;

	public SKMLExterneControleSchemaPage()
	{
		maakZoekObject();

		add(new Link<Void>("toevoegen")
		{
			@Override
			public void onClick()
			{
				setResponsePage(new SKMLExterneControleSchemaToevoegenPage(ModelUtil.ccModel(new SKMLExternSchema())));
			}
		}.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_SCHEMA_EXTERNE_CONTROLE, Actie.TOEVOEGEN)));
		add(new Link<Void>("toevoegenXls")
		{
			@Override
			public void onClick()
			{
				setResponsePage(new SKMLExterneControleSchemaXlsToevoegenPage());
			}
		}.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_SCHEMA_EXTERNE_CONTROLE, Actie.TOEVOEGEN)));
		overzicht = maakSchemaOverzicht();
		add(overzicht);
	}

	private ScreenitDataTable<SKMLExternSchema, String> maakSchemaOverzicht()
	{
		var provider = new SKMLExternSchemaProvider("deadline", SortOrder.DESCENDING, zoekModel);
		List<IColumn<SKMLExternSchema, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("SKML jaar"), SKMLExternSchema_.JAAR));
		columns.add(new PropertyColumn<>(Model.of("SKML ronde"), SKMLExternSchema_.RONDE));
		columns.add(new PropertyColumn<>(Model.of("Monster letter"), SKMLExternSchema_.LETTER));
		columns.add(new DateTimePropertyColumn<>(Model.of("Deadline"), SKMLExternSchema_.DEADLINE, new SimpleDateFormat("dd-MM-yyyy")));
		columns.add(new AbstractColumn<>(Model.of(""))
		{

			@Override
			public void populateItem(Item<ICellPopulator<SKMLExternSchema>> cellItem, String componentId, IModel<SKMLExternSchema> rowModel)
			{
				var linkPanel = new ScreenitIndicatingAjaxLinkPanel<>(componentId, rowModel, "Verwijderen")
				{

					@Override
					public void onClick(AjaxRequestTarget target, IModel<SKMLExternSchema> model)
					{
						hibernateService.delete(model.getObject());
						var nieuwOverzicht = maakSchemaOverzicht();
						overzicht.replaceWith(nieuwOverzicht);
						overzicht = nieuwOverzicht;
						target.add(overzicht);
						info("SKML Extern Controle Schema is verwijderd.");
					}
				};
				linkPanel.setOutputMarkupId(true);
				cellItem.add(linkPanel);
				linkPanel.setVisible(
					ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_SCHEMA_EXTERNE_CONTROLE, Actie.VERWIJDEREN) && schemaService.magSchemaVerwijderdWorden(
						linkPanel.getModelObject()));
			}
		});
		var overzicht = new ScreenitDataTable<>("overzicht", columns, provider, Model.of("schema's"))
		{
			@Override
			protected boolean isRowClickable(IModel<SKMLExternSchema> model)
			{
				return false;
			}
		};
		overzicht.setOutputMarkupId(true);
		return overzicht;
	}

	private void maakZoekObject()
	{
		var zoekObject = new SKMLExternSchema();
		zoekObject.setActief(true);
		zoekModel = ModelUtil.sModel(zoekObject);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(zoekModel);
	}
}
