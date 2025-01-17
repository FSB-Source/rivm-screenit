package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie;

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
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.service.mamma.MammaVisitatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ExportToXslLink;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.NotClickableAbstractColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBeTabelCounterPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gebruiker_;
import nl.rivm.screenit.model.InstellingGebruiker_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaMammografie_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek_;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderdeel;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
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
import static org.springframework.data.domain.Sort.Direction.ASC;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_VISITATIE },
	organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public abstract class MammaVisitatieOnderzoekenWerklijstPage extends MammaVisitatieBasePage
{
	static final String SESSION_KEY = "visitatie";

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	@SpringBean
	private MammaVisitatieService visitatieService;

	private final IModel<MammaVisitatieOnderzoekenWerklijstZoekObject> zoekObjectModel;

	private WebMarkupContainer refreshContainer;

	public MammaVisitatieOnderzoekenWerklijstPage(MammaVisitatieOnderdeel onderdeel)
	{
		if (ScreenitSession.get().isZoekObjectGezetForComponent(SESSION_KEY))
		{
			zoekObjectModel = (IModel<MammaVisitatieOnderzoekenWerklijstZoekObject>) ScreenitSession.get().getZoekObject(SESSION_KEY);
			zoekObjectModel.getObject().setOnderdeel(onderdeel);
		}
		else
		{
			throw new IllegalStateException("Zoekobject moet gevuld zijn met " + SESSION_KEY);
		}
		wijzigIDS7Role(onderdeel.getIds7Role());
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(CssHeaderItem.forUrl("assets/css/base_styles_be.css"));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		var onderzoekProperty = propertyChain(MammaVisitatieOnderzoek_.BEOORDELING, MammaBeoordeling_.ONDERZOEK);
		var screeningsEenheidProperty = propertyChain(onderzoekProperty, MammaOnderzoek_.SCREENINGS_EENHEID);
		var medewerkerProperty = propertyChain(onderzoekProperty, MammaOnderzoek_.MAMMOGRAFIE, MammaMammografie_.AFGEROND_DOOR, InstellingGebruiker_.MEDEWERKER);
		var persoonProperty = propertyChain(onderzoekProperty, MammaOnderzoek_.AFSPRAAK, MammaAfspraak_.UITNODIGING, MammaUitnodiging_.SCREENING_RONDE,
			MammaScreeningRonde_.DOSSIER, MammaDossier_.CLIENT, Client_.PERSOON);

		MammaVisitatieOnderzoekenDataProvider onderzoekDataProvider = new MammaVisitatieOnderzoekenDataProvider(propertyChain(onderzoekProperty, MammaOnderzoek_.CREATIE_DATUM),
			zoekObjectModel);

		add(new Label("naam", getVisitatie().getOmschrijving()));
		refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);
		addOnderzoekToevoegenButton();

		List<IColumn<MammaVisitatieOnderzoek, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<>(Model.of("Onderzoeksdatum"), "beoordeling.onderzoek.creatieDatum", propertyChain(onderzoekProperty, MammaOnderzoek_.CREATIE_DATUM),
			Constants.getDateTimeSecondsFormat()));
		if (zoekObjectModel.getObject().getOnderdeel() == MammaVisitatieOnderdeel.INSTELTECHNIEK)
		{
			columns.add(new PropertyColumn<>(Model.of("MBBer code"), propertyChain(medewerkerProperty, Gebruiker_.MEDEWERKERCODE),
				"beoordeling.onderzoek.mammografie.afgerondDoor.medewerker.medewerkercode"));
		}
		columns.add(new PropertyColumn<>(Model.of("Volgnummer"), MammaVisitatieOnderzoek_.VOLGNUMMER, "volgnummer"));
		if (ScreenitSession.get().getInstelling().getOrganisatieType() != OrganisatieType.KWALITEITSPLATFORM)
		{
			columns.add(new ClientColumn<>(propertyChain(persoonProperty, GbaPersoon_.ACHTERNAAM), "beoordeling.onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client"));
			columns.add(new GeboortedatumColumn<>(propertyChain(persoonProperty, GbaPersoon_.GEBOORTEDATUM),
				"beoordeling.onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
			columns.add(new PropertyColumn<>(Model.of("BSN"), propertyChain(persoonProperty, GbaPersoon_.BSN),
				"beoordeling.onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon.bsn"));
		}
		columns.add(new PropertyColumn<>(Model.of("SE"), propertyChain(screeningsEenheidProperty, MammaScreeningsEenheid_.NAAM), "beoordeling.onderzoek.screeningsEenheid.naam"));
		columns.add(new EnumPropertyColumn<>(Model.of("Status"), MammaVisitatieOnderzoek_.STATUS, "status", this));

		if (!ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.BEOORDELINGSEENHEID)
			&& !ScreenitSession.get().getInstelling().getOrganisatieType().equals(OrganisatieType.KWALITEITSPLATFORM)
			&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_VISITATIE, Actie.VERWIJDEREN))
		{
			columns.add(getOnderzoekVerwijderenColumn());
		}

		ScreenitDataTable<MammaVisitatieOnderzoek, String> table = new ScreenitDataTable<>("resultaten", columns, onderzoekDataProvider, 10, Model.of("onderzoek(en)"))
		{

			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaVisitatieOnderzoek> model)
			{
				if (ScreenitSession.get().getInstelling().getOrganisatieType() == OrganisatieType.KWALITEITSPLATFORM)
				{
					openBesprekenScherm(target, model, onderzoekDataProvider.getSort().getProperty());
				}
			}

			@Override
			public Panel getCustomPanel(String id)
			{
				IModel<Integer> gezienModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return (int) visitatieService.countAantalGezien(getVisitatie(), zoekObjectModel.getObject().getOnderdeel());
					}

				};

				IModel<Integer> nietGezienModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return (int) getItemCount() - gezienModel.getObject();
					}
				};

				return new MammaBeTabelCounterPanel(id, nietGezienModel, gezienModel);
			}
		};
		refreshContainer.add(table);
		refreshContainer.add(new ExportToXslLink<>("csv", "Onderzoek(en)", table));

		addVisitatieAfrondenButton();
	}

	private IColumn<MammaVisitatieOnderzoek, String> getOnderzoekVerwijderenColumn()
	{
		return new NotClickableAbstractColumn<>(Model.of("Verwijderen"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaVisitatieOnderzoek>> cellItem, String componentId, IModel<MammaVisitatieOnderzoek> rowModel)
			{
				cellItem.add(new AjaxImageCellPanel<>(componentId, rowModel, "icon-trash")
				{
					@Override
					protected void onClick(AjaxRequestTarget target)
					{
						MammaVisitatieOnderzoek visitatieOnderzoek = rowModel.getObject();
						if (getVisitatie().getStatus() != MammaVisitatieStatus.UITGEVOERD)
						{
							if (MammaVisitatieOnderzoekStatus.NIET_GEZIEN.equals(visitatieOnderzoek.getStatus()))
							{
								dialog.openWith(target,
									new MammaVisitatieOnderzoekVerwijderenPopupPanel(BootstrapDialog.CONTENT_ID, new CompoundPropertyModel<>(ModelUtil.sModel(visitatieOnderzoek)))
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
								GbaPersoon persoon = visitatieOnderzoek.getBeoordeling().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient()
									.getPersoon();
								warn(String.format(getString("error.verwijderen"), persoon.getBsn(), DateUtil.getGeboortedatum(persoon), getString("error.verwijderen.besproken")));
							}
						}
						else
						{
							GbaPersoon persoon = visitatieOnderzoek.getBeoordeling().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient()
								.getPersoon();
							warn(String.format(getString("error.verwijderen"), persoon.getBsn(), DateUtil.getGeboortedatum(persoon), getString("error.visitatie.afgerond")));
						}
					}
				});
			}
		};
	}

	private MammaVisitatie getVisitatie()
	{
		return zoekObjectModel.getObject().getVisitatie();
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
			&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_VISITATIE, Actie.TOEVOEGEN)));
	}

	private void openOnderzoekToevoegenPopupPanel(AjaxRequestTarget target)
	{
		if (getVisitatie().getStatus() != MammaVisitatieStatus.UITGEVOERD)
		{
			dialog.openWith(target,
				new MammaVisitatieOnderzoekToevoegenPopupPanel(BootstrapDialog.CONTENT_ID, ModelUtil.cModel(getVisitatie()), zoekObjectModel.getObject().getOnderdeel())
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
			warn(getString("error.toevoegen") + getString("error.visitatie.afgerond"));
		}
	}

	private void addVisitatieAfrondenButton()
	{
		boolean kanVisitatieAfronden = visitatieService.kanVisitatieAfronden(getVisitatie());

		add(new IndicatingAjaxLink<Void>("visitatieAfronden")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				kwaliteitscontroleService.visitatieAfronden(getVisitatie());
				setVisible(false);
				target.add(this, refreshContainer);
				info(getString("visitatie.afgerond"));
			}

		}.setVisible(kanVisitatieAfronden).setOutputMarkupId(true));
	}

	private void openBesprekenScherm(AjaxRequestTarget target, IModel<MammaVisitatieOnderzoek> model, String sortProperty)
	{
		Map<Long, Long> onderzoekenIdMapping = new LinkedHashMap<>();
		var zoekObject = zoekObjectModel.getObject();
		for (var onderzoek : visitatieService.zoekVisitatieOnderzoeken(zoekObject.getOnderdeel(), zoekObject.getVisitatie(), -1, -1, Sort.by(ASC, sortProperty)))
		{
			onderzoekenIdMapping.put(onderzoek.getBeoordeling().getId(), onderzoek.getId());
		}

		MammaVisitatieOnderzoek visitatieOnderzoek = model.getObject();
		MammaVisitatie visitatie = visitatieOnderzoek.getVisitatie();
		if (visitatie.getGestartOp() == null)
		{
			kwaliteitscontroleService.startKwaliteitscontrole(visitatie);
		}
		setResponsePage(new MammaVisitatieOnderzoekInzienPage(visitatieOnderzoek.getBeoordeling().getId(), onderzoekenIdMapping, getClass()));
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = super.getContextMenuItems();
		contextMenuItems.addAll(MammaVisitatieOnderdeelWrapper.getContextMenuItems());

		return contextMenuItems;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(zoekObjectModel);
	}
}
