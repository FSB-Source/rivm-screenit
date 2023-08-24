package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieWerklijstZoekObject;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.AjaxLinkTableCellPanel;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GebruikerColumn;
import nl.rivm.screenit.main.web.component.table.NotClickableAbstractColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadColumn;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.commons.collections4.CollectionUtils;
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
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_VISITATIE },
	organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
public class MammaVisitatieOverzichtPage extends MammaVisitatieBasePage
{

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	private final boolean isKwaliteitsplatform = ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.KWALITEITSPLATFORM);

	private ScreenitDataTable<MammaVisitatie, String> overzicht;

	private IModel<MammaVisitatieWerklijstZoekObject> zoekModel;

	public MammaVisitatieOverzichtPage()
	{
		super();
		createAanmakenButton();
		createFilter();
		createOverzicht();
	}

	private void createAanmakenButton()
	{
		add(new IndicatingAjaxLink<>("visitatieToevoegen")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				IModel<MammaVisitatie> model = ModelUtil.ccModel(new MammaVisitatie());
				MammaVisitatie visitatie = model.getObject();
				visitatie.setAangemaaktDoor(ScreenitSession.get().getLoggedInInstellingGebruiker());
				visitatie.setAangemaaktOp(dateSupplier.getDate());
				visitatie.setStatus(MammaVisitatieStatus.INGEPLAND);
				openEditPopupPanel(target, model);
			}

		}.setVisible(!isKwaliteitsplatform
			&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_VISITATIE, Actie.TOEVOEGEN)));
	}

	private void createFilter()
	{
		zoekModel = new CompoundPropertyModel<>(new MammaVisitatieWerklijstZoekObject());
		if (isKwaliteitsplatform)
		{
			zoekModel.getObject().setTotMet(DateUtil.toUtilDate(dateSupplier.getLocalDate().minusMonths(3)));
		}
		defaultZoekFilter();
		add(new MammaVisitatieZoekPanel("filter", zoekModel)
		{
			@Override
			protected void onZoeken(AjaxRequestTarget target, IModel<MammaVisitatieWerklijstZoekObject> zoekModel)
			{
				defaultZoekFilter();
				target.add(overzicht);
			}

		});
	}

	private void defaultZoekFilter()
	{
		List<MammaVisitatieStatus> statussen = zoekModel.getObject().getStatussen();
		if (CollectionUtils.isEmpty(statussen))
		{
			if (isKwaliteitsplatform)
			{
				statussen.addAll(Arrays.asList(MammaVisitatieStatus.VRIJGEGEVEN, MammaVisitatieStatus.UITGEVOERD));
			}
			else
			{
				statussen.addAll(Arrays.asList(MammaVisitatieStatus.VRIJGEGEVEN, MammaVisitatieStatus.INGEPLAND));
			}
		}
	}

	private void createOverzicht()
	{
		List<IColumn<MammaVisitatie, String>> columns = new ArrayList<>();

		columns.add(new PropertyColumn<>(Model.of("Omschrijving"), "omschrijving", "omschrijving"));
		columns.add(new DateTimePropertyColumn<>(Model.of("Gestart"), "gestartOp", "gestartOp", Constants.getDateTimeFormat()));
		columns.add(new DateTimePropertyColumn<>(Model.of("Afgerond"), "afgerondOp", "afgerondOp", Constants.getDateTimeFormat()));
		columns.add(new DateTimePropertyColumn<>(Model.of("Aangemaakt"), "aangemaaktOp", "aangemaaktOp", Constants.getDateTimeFormat()));
		columns.add(new GebruikerColumn<>(Model.of("Door"), "medewerker.achternaam", "aangemaaktDoor.medewerker"));
		columns.add(new EnumPropertyColumn<>(Model.of("Status"), "status", "status", this));
		columns.add(new PropertyColumn<>(Model.of("BE"), "beoordelingsEenheid.naam", "beoordelingsEenheid.naam"));
		columns.add(new UploadDocumentDownloadColumn<>(Model.of("Rapportage"), "rapportageBijlage"));
		columns.add(new UploadDocumentDownloadColumn<>(Model.of("Vragenlijst"), "vragenlijstBijlage"));

		if (!ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.RIVM))
		{
			columns.add(new AbstractColumn<>(Model.of(""))
			{

				@Override
				public void populateItem(Item<ICellPopulator<MammaVisitatie>> cellItem, String componentId, IModel<MammaVisitatie> rowModel)
				{
					cellItem.add(new AjaxLinkTableCellPanel<>(componentId, rowModel, "button.open.visitatie")
					{

						@Override
						protected void onClick(AjaxRequestTarget target, IModel<MammaVisitatie> model)
						{
							MammaVisitatie visitatie = ModelUtil.nullSafeGet(model);
							IModel<MammaVisitatieOnderzoekenWerklijstZoekObject> zoekObjectModel = new CompoundPropertyModel<>(new MammaVisitatieOnderzoekenWerklijstZoekObject());
							zoekObjectModel.getObject().setVisitatie(visitatie);
							ScreenitSession.get().setZoekObject(MammaVisitatieOnderzoekenWerklijstPage.SESSION_KEY, zoekObjectModel);
							setResponsePage(new MammaVisitatieOnderzoekenOnderdeelInsteltechniekWerklijstPage());
						}
					});
				}
			});
		}

		if (!isKwaliteitsplatform && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_VISITATIE, Actie.VERWIJDEREN))
		{
			columns.add(getVerwijderenColumn());
		}
		MammaVisitatieProvider dataProvider = new MammaVisitatieProvider(zoekModel);
		overzicht = new ScreenitDataTable<>("werklijst", columns, dataProvider,
			Model.of("visitatie(s)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaVisitatie> model)
			{
				super.onClick(target, model);
				if (!isKwaliteitsplatform
					&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_VISITATIE, Actie.INZIEN))
				{
					if (MammaVisitatieStatus.UITGEVOERD != model.getObject().getStatus())
					{
						openEditPopupPanel(target, ModelUtil.ccModel(model.getObject()));
					}
					else
					{
						warn(getString("error.uitgevoerd"));
					}
				}
			}
		};
		overzicht.setOutputMarkupId(true);
		add(overzicht);

	}

	private IColumn<MammaVisitatie, String> getVerwijderenColumn()
	{
		return new NotClickableAbstractColumn<>(Model.of(""))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaVisitatie>> cellItem, String componentId, IModel<MammaVisitatie> rowModel)
			{
				cellItem.add(new AjaxImageCellPanel<>(componentId, rowModel, "icon-trash")
				{

					@Override
					protected void onClick(AjaxRequestTarget target)
					{
						MammaVisitatie visitatie = getModelObject();
						dialog.openWith(target, new MammaVisitatieVerwijderenPopupPanel(IDialog.CONTENT_ID, ModelUtil.csModel(visitatie))
						{
							@Override
							protected void onOpslaanSuccesvol(AjaxRequestTarget target)
							{
								dialog.close(target);
								target.add(overzicht);
							}
						});
					}
				});
			}
		};
	}

	private void openEditPopupPanel(AjaxRequestTarget target, IModel<MammaVisitatie> model)
	{
		dialog.openWith(target, new MammaVisitatieEditPopupPanel(IDialog.CONTENT_ID, model)
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
