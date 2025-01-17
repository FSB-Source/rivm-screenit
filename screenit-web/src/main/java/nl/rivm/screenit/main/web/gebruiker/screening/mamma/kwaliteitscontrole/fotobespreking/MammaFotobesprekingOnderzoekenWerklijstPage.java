package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.service.mamma.MammaFotobesprekingService;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.NotClickableAbstractColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBeTabelCounterPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst.MammaBeoordelingenWerklijstPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.data.domain.Sort;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_FOTOBESPREKING },
	organisatieTypeScopes = { OrganisatieType.BEOORDELINGSEENHEID, OrganisatieType.SCREENINGSORGANISATIE },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaFotobesprekingOnderzoekenWerklijstPage extends MammaFotobesprekingBasePage
{

	static final String SESSION_KEY = "fotobespreking";

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	@SpringBean
	private MammaFotobesprekingService fotobesprekingService;

	@SpringBean
	private HibernateService hibernateService;

	private final IModel<MammaFotobesprekingOnderzoekenWerklijstZoekObject> zoekObjectModel;

	private WebMarkupContainer refreshContainer;

	public MammaFotobesprekingOnderzoekenWerklijstPage()
	{
		if (ScreenitSession.get().isZoekObjectGezetForComponent(SESSION_KEY))
		{
			zoekObjectModel = (IModel<MammaFotobesprekingOnderzoekenWerklijstZoekObject>) ScreenitSession.get().getZoekObject(SESSION_KEY);
		}
		else
		{
			throw new IllegalStateException("Zoekobject moet gevuld zijn met " + SESSION_KEY);
		}
		addOnderzoekToevoegenButton();
		wijzigIDS7Role(getFotobespreking().getRole());
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		MammaFotobesprekingOnderzoekenDataProvider onderzoekDataProvider = new MammaFotobesprekingOnderzoekenDataProvider(
			propertyChain(MammaFotobesprekingOnderzoek_.BEOORDELING, MammaBeoordeling_.ONDERZOEK, MammaOnderzoek_.CREATIE_DATUM), zoekObjectModel);

		add(new Label("naam", getFotobespreking().getOmschrijving()));

		refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		var onderzoekProperty = propertyChain(MammaFotobesprekingOnderzoek_.BEOORDELING, MammaBeoordeling_.ONDERZOEK);
		var persoonProperty = propertyChain(onderzoekProperty, MammaOnderzoek_.AFSPRAAK, MammaAfspraak_.UITNODIGING, MammaUitnodiging_.SCREENING_RONDE,
			MammaScreeningRonde_.DOSSIER, MammaDossier_.CLIENT, Client_.PERSOON);

		List<IColumn<MammaFotobesprekingOnderzoek, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<>(Model.of("Onderzoeksdatum"), "beoordeling.onderzoek.creatieDatum",
			propertyChain(onderzoekProperty, MammaOnderzoek_.CREATIE_DATUM), Constants.getDateTimeSecondsFormat()));
		columns.add(new PropertyColumn<>(Model.of("Volgnummer"), MammaFotobesprekingOnderzoek_.VOLGNUMMER, "volgnummer"));

		if (!MammobridgeRole.anoniemeRollen().contains(ScreenitSession.get().getMammaHuidigeIDS7Role())
			|| ScreenitSession.get().getInstelling().getOrganisatieType() != OrganisatieType.BEOORDELINGSEENHEID)
		{
			columns.add(new ClientColumn<>(propertyChain(persoonProperty, GbaPersoon_.ACHTERNAAM), "beoordeling.onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client"));
			columns.add(
				new GeboortedatumColumn<>(propertyChain(persoonProperty, GbaPersoon_.GEBOORTEDATUM),
					"beoordeling.onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
			columns.add(new PropertyColumn<>(Model.of("BSN"), propertyChain(persoonProperty, GbaPersoon_.BSN),
				"beoordeling.onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon.bsn"));
		}

		columns.add(new PropertyColumn<>(Model.of("SE"), propertyChain(onderzoekProperty, MammaOnderzoek_.SCREENINGS_EENHEID, MammaScreeningsEenheid_.NAAM),
			"beoordeling.onderzoek.screeningsEenheid.naam"));
		columns.add(new EnumPropertyColumn<>(Model.of("Status"), MammaFotobesprekingOnderzoek_.STATUS, "status", this));

		if (!ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.BEOORDELINGSEENHEID)
			&& !ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.KWALITEITSPLATFORM)
			&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_FOTOBESPREKING, Actie.VERWIJDEREN))
		{
			columns.add(getOnderzoekVerwijderenColumn());
		}

		refreshContainer.add(new ScreenitDataTable<>("resultaten", columns, onderzoekDataProvider, 10, Model.of("onderzoek(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaFotobesprekingOnderzoek> model)
			{
				if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_FOTOBESPREKING, Actie.INZIEN) && isHeeftImsKoppelingRecht())
				{
					openBesprekenScherm(target, model, onderzoekDataProvider.getSort().getProperty());
				}
			}

			@Override
			public Panel getCustomPanel(String id)
			{
				IModel<Integer> besprokenModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return (int) fotobesprekingService.getAantalBesproken(getFotobespreking());
					}
				};

				IModel<Integer> teBesprekenModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return (int) getItemCount() - besprokenModel.getObject();
					}
				};

				return new MammaBeTabelCounterPanel(id, teBesprekenModel, besprokenModel);
			}
		});

		addFotobesprekingAfrondenButton();
	}

	private IColumn<MammaFotobesprekingOnderzoek, String> getOnderzoekVerwijderenColumn()
	{
		return new NotClickableAbstractColumn<>(Model.of("Verwijderen"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaFotobesprekingOnderzoek>> cellItem, String componentId, IModel<MammaFotobesprekingOnderzoek> rowModel)
			{
				cellItem.add(new AjaxImageCellPanel<>(componentId, rowModel, "icon-trash")
				{
					@Override
					protected void onClick(AjaxRequestTarget target)
					{
						MammaFotobesprekingOnderzoek fotobesprekingOnderzoek = getModelObject();
						if (fotobesprekingOnderzoek.getFotobespreking().getAfgerondOp() == null)
						{
							if (MammaFotobesprekingOnderzoekStatus.NIET_BESPROKEN.equals(fotobesprekingOnderzoek.getStatus()))
							{
								dialog.openWith(target,
									new MammaFotobesprekingOnderzoekVerwijderenPopupPanel(IDialog.CONTENT_ID,
										new CompoundPropertyModel<>(ModelUtil.sModel(fotobesprekingOnderzoek)))
									{
										@Override
										protected void onOpslaanSuccesvol(AjaxRequestTarget target)
										{
											dialog.close(target);
											target.add(refreshContainer);
										}
									});
							}
							else
							{
								GbaPersoon persoon = fotobesprekingOnderzoek.getBeoordeling().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier()
									.getClient()
									.getPersoon();
								warn(String.format(getString("error.verwijderen"), persoon.getBsn(), DateUtil.getGeboortedatum(persoon), getString("error.verwijderen.besproken")));
							}
						}
						else
						{
							GbaPersoon persoon = fotobesprekingOnderzoek.getBeoordeling().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient()
								.getPersoon();
							warn(String.format(getString("error.verwijderen"), persoon.getBsn(), DateUtil.getGeboortedatum(persoon), getString("error.fotobespreking.afgerond")));
						}
					}
				});
			}
		};
	}

	private void addOnderzoekToevoegenButton()
	{
		add(new IndicatingAjaxLink<Void>("onderzoekToevoegen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				openOnderzoekToevoegenPopupPanel(target);
			}

		}.setVisible(!ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.BEOORDELINGSEENHEID)
			&& !ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.KWALITEITSPLATFORM)
			&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_FOTOBESPREKING, Actie.TOEVOEGEN)));
	}

	private void addFotobesprekingAfrondenButton()
	{
		boolean kanFotobesprekingAfronden = fotobesprekingService.kanFotobesprekingAfronden(getFotobespreking())
			&& OrganisatieType.BEOORDELINGSEENHEID.equals(ScreenitSession.get().getInstelling().getOrganisatieType());

		add(new ConfirmingIndicatingAjaxLink<Void>("besprekingAfronden", dialog, "confirm.fotobespreking.afronden")
		{
			private boolean popupNietNodig = !fotobesprekingService.nieuweBeoordelingenAangevraagdNavFotobespreking(getFotobespreking());

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				afronden(target);
				if (popupNietNodig)
				{
					info(getString("fotobespreking.afgerond"));
				}
				else
				{
					setResponsePage(MammaBeoordelingenWerklijstPage.class);
				}
			}

			private void afronden(AjaxRequestTarget target)
			{
				kwaliteitscontroleService.fotobesprekingAfronden(getFotobespreking());
				setVisible(false);
				target.add(this, refreshContainer);
			}

			@Override
			protected boolean skipConfirmation()
			{
				return popupNietNodig;
			}

			@Override
			protected void onNo(AjaxRequestTarget target)
			{
				afronden(target);
				info(getString("fotobespreking.afgerond"));
			}

		}.setVisible(kanFotobesprekingAfronden).setOutputMarkupId(true));
	}

	private MammaFotobespreking getFotobespreking()
	{
		return zoekObjectModel.getObject().getFotobespreking();
	}

	private void openOnderzoekToevoegenPopupPanel(AjaxRequestTarget target)
	{
		if (getFotobespreking().getAfgerondOp() == null)
		{
			dialog.openWith(target, new MammaFotobesprekingOnderzoekToevoegenPopupPanel(IDialog.CONTENT_ID, ModelUtil.cModel(getFotobespreking()))
			{
				@Override
				protected void onOpslaanSuccesvol(AjaxRequestTarget target)
				{
					dialog.close(target);
					target.add(refreshContainer);
				}
			});
		}
		else
		{
			warn(getString("error.toevoegen") + getString("error.fotobespreking.afgerond"));
		}
	}

	private void openBesprekenScherm(AjaxRequestTarget target, IModel<MammaFotobesprekingOnderzoek> model, String sortProperty)
	{
		Map<Long, Long> onderzoekenIdMapping = new LinkedHashMap<>();
		var sort = Sort.by(sortProperty);
		for (MammaFotobesprekingOnderzoek onderzoek : fotobesprekingService.zoekFotobesprekingOnderzoeken(zoekObjectModel.getObject(), -1, -1, sort))
		{
			onderzoekenIdMapping.put(onderzoek.getBeoordeling().getId(), onderzoek.getId());
		}

		if (model.getObject().getFotobespreking().getGestartOp() == null)
		{
			kwaliteitscontroleService.startKwaliteitscontrole(model.getObject().getFotobespreking());
		}
		setResponsePage(new MammaFotobesprekingOnderzoekBesprekenPage(model.getObject().getBeoordeling().getId(), onderzoekenIdMapping, getClass()));
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = super.getContextMenuItems();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.fotobespreking.onderzoeken", false,
			MammaFotobesprekingOnderzoekenWerklijstPage.class));

		return contextMenuItems;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(zoekObjectModel);
	}
}
