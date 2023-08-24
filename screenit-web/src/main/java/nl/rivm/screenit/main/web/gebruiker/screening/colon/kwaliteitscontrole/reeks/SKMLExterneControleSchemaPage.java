
package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole.reeks;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Transient;

import nl.rivm.screenit.main.service.SKMLExternSchemaService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxLinkPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
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

	private static final long serialVersionUID = 1L;

	@Transient
	private IModel<SKMLExternSchema> zoekModel;

	@SpringBean
	private AutorisatieService authService;

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

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(new SKMLExterneControleSchemaToevoegenPage(ModelUtil.cModel(new SKMLExternSchema())));
			}

			@Override
			public boolean isVisible()
			{
				ToegangLevel level = authService.getToegangLevel(ScreenitSession.get().getLoggedInInstellingGebruiker(), Actie.AANPASSEN, true,
					Recht.GEBRUIKER_BEHEER_SCHEMA_EXTERNE_CONTROLE);
				return level != null && ToegangLevel.LANDELIJK == level;
			}
		});
		add(new Link<Void>("toevoegenXls")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(new SKMLExterneControleSchemaXlsToevoegenPage());
			}

			@Override
			public boolean isVisible()
			{
				ToegangLevel level = authService.getToegangLevel(ScreenitSession.get().getLoggedInInstellingGebruiker(), Actie.AANPASSEN, true,
					Recht.GEBRUIKER_BEHEER_SCHEMA_EXTERNE_CONTROLE);
				return level != null && ToegangLevel.LANDELIJK == level;
			}
		});
		overzicht = maakSchemaOverzicht();
		add(overzicht);
	}

	private ScreenitDataTable<SKMLExternSchema, String> maakSchemaOverzicht()
	{
		SKMLExternSchemaProvider provider = new SKMLExternSchemaProvider("deadline", SortOrder.DESCENDING, zoekModel);
		List<IColumn<SKMLExternSchema, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<SKMLExternSchema, String>(Model.of("SKML jaar"), "jaar"));
		columns.add(new PropertyColumn<SKMLExternSchema, String>(Model.of("SKML ronde"), "ronde"));
		columns.add(new PropertyColumn<SKMLExternSchema, String>(Model.of("Monster letter"), "letter"));
		columns.add(new DateTimePropertyColumn<SKMLExternSchema, String>(Model.of("Deadline"), "deadline", new SimpleDateFormat("dd-MM-yyyy")));
		columns.add(new AbstractColumn<SKMLExternSchema, String>(Model.of(""))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<SKMLExternSchema>> cellItem, String componentId, IModel<SKMLExternSchema> rowModel)
			{
				ScreenitIndicatingAjaxLinkPanel<SKMLExternSchema> linkPanel = new ScreenitIndicatingAjaxLinkPanel<SKMLExternSchema>(componentId, rowModel, "Verwijderen")
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target, IModel<SKMLExternSchema> model)
					{
						hibernateService.delete(model.getObject());
						ScreenitDataTable<SKMLExternSchema, String> nieuwOverzicht = maakSchemaOverzicht();
						overzicht.replaceWith(nieuwOverzicht);
						overzicht = nieuwOverzicht;
						target.add(overzicht);
						info("SKML Extern Controle Schema is verwijderd.");
					}

					@Override
					public boolean isVisible()
					{
						ToegangLevel level = authService.getToegangLevel(ScreenitSession.get().getLoggedInInstellingGebruiker(), Actie.VERWIJDEREN, true,
							Recht.GEBRUIKER_BEHEER_SCHEMA_EXTERNE_CONTROLE);
						return level != null && ToegangLevel.LANDELIJK == level && schemaService.magSchemaVerwijderdWorden(getModelObject());
					}
				};
				linkPanel.setOutputMarkupId(true);
				cellItem.add(linkPanel);
			}
		});
		ScreenitDataTable<SKMLExternSchema, String> overzicht = new ScreenitDataTable<SKMLExternSchema, String>("overzicht", columns, provider, Model.of("schema's"))
		{

			private static final long serialVersionUID = 1L;

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
		SKMLExternSchema zoekObject = new SKMLExternSchema();
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
