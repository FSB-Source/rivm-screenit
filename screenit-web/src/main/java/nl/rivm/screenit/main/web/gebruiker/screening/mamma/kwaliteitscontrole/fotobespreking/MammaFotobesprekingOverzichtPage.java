package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

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
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingWerklijstZoekObject;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.AjaxLinkTableCellPanel;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GebruikerColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_FOTOBESPREKING },
	organisatieTypeScopes = { OrganisatieType.BEOORDELINGSEENHEID, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
public class MammaFotobesprekingOverzichtPage extends MammaFotobesprekingBasePage
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaFotobesprekingOverzichtPage.class);

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	private ScreenitDataTable<MammaFotobespreking, String> overzicht;

	private IModel<MammaFotobesprekingWerklijstZoekObject> zoekModel;

	public MammaFotobesprekingOverzichtPage()
	{
		super();
		createAanmakenButton();
		createFilter();
		createOverzicht();
	}

	private void createAanmakenButton()
	{
		add(new IndicatingAjaxLink<Object>("fotobesprekingToevoegen")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				IModel<MammaFotobespreking> model = ModelUtil.cModel(new MammaFotobespreking());
				model.getObject().setAangemaaktDoor(ScreenitSession.get().getLoggedInInstellingGebruiker());
				model.getObject().setAangemaaktOp(dateSupplier.getDate());
				openEditPopupPanel(target, model);
			}

		}.setVisible(!ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.BEOORDELINGSEENHEID)
			&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_FOTOBESPREKING, Actie.TOEVOEGEN)));
	}

	private void createFilter()
	{
		zoekModel = new CompoundPropertyModel<>(new MammaFotobesprekingWerklijstZoekObject());
		if (ScreenitSession.get().getInstelling().getOrganisatieType() == OrganisatieType.BEOORDELINGSEENHEID)
		{
			zoekModel.getObject().setTotMet(DateUtil.toUtilDate(dateSupplier.getLocalDate().minusMonths(3)));
		}
		add(new MammaFotobesprekingZoekPanel("filter", zoekModel)
		{
			@Override
			protected void onZoeken(AjaxRequestTarget target, IModel<MammaFotobesprekingWerklijstZoekObject> zoekModel)
			{
				target.add(overzicht);
			}
		});
	}

	private void createOverzicht()
	{
		List<IColumn<MammaFotobespreking, String>> columns = new ArrayList<>();

		columns.add(new PropertyColumn<>(Model.of("Omschrijving"), "omschrijving", "omschrijving"));
		columns.add(new DateTimePropertyColumn<>(Model.of("Gestart"), "gestartOp", "gestartOp", Constants.getDateTimeFormat()));
		columns.add(new DateTimePropertyColumn<>(Model.of("Afgerond"), "afgerondOp", "afgerondOp", Constants.getDateTimeFormat()));
		columns.add(new DateTimePropertyColumn<>(Model.of("Aangemaakt"), "aangemaaktOp", "aangemaaktOp", Constants.getDateTimeFormat()));
		columns.add(new GebruikerColumn<>(Model.of("Door"), "medewerker.achternaam", "aangemaaktDoor.medewerker"));
		columns.add(new EnumPropertyColumn<>(Model.of("Type"), "type", "type", this));
		columns.add(new EnumPropertyColumn<>(Model.of("Role"), "role", "role", this));
		columns.add(new PropertyColumn<>(Model.of("BE"), "beoordelingsEenheid.naam", "beoordelingsEenheid.naam"));
		columns.add(new PropertyColumn<>(Model.of("SE"), "screeningsEenheid.naam", "screeningsEenheid.naam"));
		if (!ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.RIVM))
		{
			columns.add(new AbstractColumn<MammaFotobespreking, String>(Model.of(""))
			{

				@Override
				public void populateItem(Item<ICellPopulator<MammaFotobespreking>> cellItem, String componentId, IModel<MammaFotobespreking> rowModel)
				{
					cellItem.add(new AjaxLinkTableCellPanel<MammaFotobespreking>(componentId, rowModel, "button.open.fotobespreking")
					{

						@Override
						protected void onClick(AjaxRequestTarget target, IModel<MammaFotobespreking> rowModel)
						{
							IModel<MammaFotobesprekingOnderzoekenWerklijstZoekObject> zoekObjectModel = new CompoundPropertyModel<>(
								new MammaFotobesprekingOnderzoekenWerklijstZoekObject());
							zoekObjectModel.getObject().setFotobespreking(ModelUtil.nullSafeGet(rowModel));
							ScreenitSession.get().setZoekObject(MammaFotobesprekingOnderzoekenWerklijstPage.SESSION_KEY, zoekObjectModel);
							setResponsePage(MammaFotobesprekingOnderzoekenWerklijstPage.class);
						}
					});
				}
			});
		}
		MammaFotobesprekingProvider dataProvider = new MammaFotobesprekingProvider(zoekModel);
		overzicht = new ScreenitDataTable<MammaFotobespreking, String>("werklijst", columns, dataProvider,
			Model.of("fotobespreking(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaFotobespreking> model)
			{
				super.onClick(target, model);
				if (!ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.BEOORDELINGSEENHEID)
					&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_FOTOBESPREKING, Actie.INZIEN))
				{
					openEditPopupPanel(target, ModelUtil.cModel(model.getObject()));
				}
			}
		};
		overzicht.setOutputMarkupId(true);
		add(overzicht);

	}

	private void openEditPopupPanel(AjaxRequestTarget target, IModel<MammaFotobespreking> model)
	{
		dialog.openWith(target, new MammaFotobesprekingEditPopupPanel(BootstrapDialog.CONTENT_ID, model)
		{

			@Override
			protected void onOpslaanSuccesvol(AjaxRequestTarget target)
			{
				dialog.close(target);
				target.add(overzicht);
			}

		});
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(zoekModel);
	}
}
