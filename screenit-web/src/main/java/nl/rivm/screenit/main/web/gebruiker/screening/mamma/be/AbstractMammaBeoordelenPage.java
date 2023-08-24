package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

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

import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.model.mamma.MammaImsUserSessionType;
import nl.rivm.screenit.main.model.mamma.beoordeling.BeoordelingenReserveringResult;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.service.mamma.MammaImsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst.MiniWerklijstPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammobridgeFocusMode;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.RestartResponseException;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_BEOORDELING_WERKLIJST, Recht.GEBRUIKER_FOTOBESPREKING, Recht.GEBRUIKER_VISITATIE, Recht.GEBRUIKER_AD_HOC_MEEMKIJKVERZOEK_WERKLIJST },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
@Slf4j
public abstract class AbstractMammaBeoordelenPage extends AbstractMammaBePage
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	protected MammaBeoordelingService beoordelingService;

	@SpringBean
	protected MammaBaseBeoordelingService baseBeoordelingService;

	@SpringBean
	protected MammaImsService imsService;

	private IModel<MammaBeoordeling> beoordelingModel;

	private List<Long> volgendeGereserveerdeBeoordelingenIds;

	private final List<Long> beoordelingenIds;

	private final Class<? extends MammaScreeningBasePage> werklijstPageClass;

	protected final WebMarkupContainer dossierContainer;

	private final Long initieleBeoordelingId;

	protected AbstractMammaBeoordelenPage(Long initieleBeoordelingId, List<Long> beoordelingenIds, Class<? extends MammaScreeningBasePage> werklijstPageClass)
	{
		this.initieleBeoordelingId = initieleBeoordelingId;
		this.beoordelingenIds = beoordelingenIds;
		this.werklijstPageClass = werklijstPageClass;

		dossierContainer = new WebMarkupContainer("dossierContainer");
		dossierContainer.setOutputMarkupId(true);
		add(dossierContainer);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		openInitieleBeoordeling(initieleBeoordelingId);
	}

	@Override
	protected void onConfigure()
	{
		super.onConfigure();

		var huidigeBeoordeling = getModelObject();
		if (huidigeBeoordeling != null && !beoordelingReserveringService.gereserveerdVoorGebruiker(huidigeBeoordeling.getId(), getIngelogdeGebruiker(), getLezerSoort()))
		{
			LOG.info("beoordelingId: '{}' niet gereserveerd voor ingelogde gebruiker (waarschijnlijk terugknop browser gebruikt)", huidigeBeoordeling.getId());
			ScreenitSession.get().warn("Onderzoek kan niet meer geopend worden via terugknop");
			throw new RestartResponseException(werklijstPageClass);
		}
	}

	protected void openInitieleBeoordeling(Long initieleBeoordelingId)
	{
		BeoordelingenReserveringResult reserveringResult = beoordelingService.openBeschikbareBeoordeling(initieleBeoordelingId, beoordelingenIds, getIngelogdeGebruiker(),
			getLezerSoort());

		if (reserveringResult.getInfoMessage() != null)
		{
			ScreenitSession.get().info(reserveringResult.getInfoMessage());
		}

		if (reserveringResult.getEersteGereserveerdeBeoordelingId() == null)
		{
			throw new RestartResponseException(werklijstPageClass);
		}

		updateBeoordelingModel(reserveringResult);
		updateClientAfhankelijkePanels();
	}

	private void updateBeoordelingModel(BeoordelingenReserveringResult reserveringResult)
	{
		beoordelingModel = ModelUtil.ccModel(hibernateService.get(MammaBeoordeling.class, reserveringResult.getEersteGereserveerdeBeoordelingId()));
		volgendeGereserveerdeBeoordelingenIds = reserveringResult.getVolgendeGereserveerdeBeoordelingenIds();

		logBeoordelingIngezien();

		if (getModelObject().getStatus() == MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN || getModelObject().getStatus() == MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN)
		{
			ScreenitSession.get().info(getString("beoordeling.al.opgeslagen"));
		}
	}

	protected void logBeoordelingIngezien()
	{
		beoordelingService.logBeoordelingIngezien(beoordelingModel.getObject(), getIngelogdeGebruiker(), false);
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forScript(createImsDesktopSyncCommand(), null));
		response.render(JavaScriptHeaderItem.forScript(createImsOpenWebsocketCommand(), null));
		response.render(JavaScriptHeaderItem.forScript(createImsUpdateBsnCommand(), null));
		response.render(JavaScriptHeaderItem.forScript(createImsAllImagesSeenCommand(), null));
		response.render(JavaScriptHeaderItem.forScript(createEventListenerBeforeUnload(), null));
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.TRUE;
	}

	private void updateClientAfhankelijkePanels()
	{
		maakPaspoort();
		maakMiniWerklijst();
		maakRondesContainer(beoordelingModel);
	}

	protected abstract void maakRondesContainer(IModel<MammaBeoordeling> beoordelingModel);

	protected void addRondeHistorie(List<AbstractMammaRondePanel> rondePanels)
	{
		List<MammaBeoordeling> historischeBeoordelingen = beoordelingService.getVorigeTweeTeTonenBeoordelingen(beoordelingModel.getObject());
		for (MammaBeoordeling beoordeling : historischeBeoordelingen)
		{
			MammaBeoordelenHistorischeRondePanel readOnlyRondePanel = new MammaBeoordelenHistorischeRondePanel("rondeItem", ModelUtil.sModel(beoordeling));
			rondePanels.add(readOnlyRondePanel);
		}
		fillRondesInContainer(rondePanels);
	}

	protected void fillRondesInContainer(List<? extends AbstractBEAccordionPanel<?>> rondePanels)
	{
		RepeatingView rondeRepeatingView = new RepeatingView("rondesContainer");
		for (AbstractBEAccordionPanel<?> panel : rondePanels)
		{
			WebMarkupContainer rondeItemWebMarkupContainer = new WebMarkupContainer(rondeRepeatingView.newChildId());
			rondeRepeatingView.add(rondeItemWebMarkupContainer);
			rondeItemWebMarkupContainer.add(panel);
			panel.setOutputMarkupId(true);
			panel.setOutputMarkupPlaceholderTag(true);
		}
		dossierContainer.addOrReplace(rondeRepeatingView);
	}

	protected void maakPaspoort()
	{
		dossierContainer.addOrReplace(getClientPaspoortPanel("paspoort"));
	}

	protected Panel getClientPaspoortPanel(String id)
	{
		return new MammaClientPaspoortPanel(id, new PropertyModel<>(getModel(), "onderzoek.afspraak.uitnodiging.screeningRonde"), false);
	}

	protected void maakMiniWerklijst()
	{
		dossierContainer.addOrReplace(getMiniWerklijst("miniwerklijst"));
	}

	protected Panel getMiniWerklijst(String id)
	{
		return new MiniWerklijstPanel(id, this, huidigeBeoordelingId(), beoordelingenIds);
	}

	protected Long huidigeOnderzoekId()
	{
		return getOnderzoek().getId();
	}

	protected Long huidigeBeoordelingId()
	{
		return getModelObject().getId();
	}

	protected MammaOnderzoek getOnderzoek()
	{
		return getModelObject().getOnderzoek();
	}

	protected IModel<MammaBeoordeling> getModel()
	{
		return beoordelingModel;
	}

	private MammaBeoordeling getModelObject()
	{
		return ModelUtil.nullSafeGet(beoordelingModel);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(beoordelingModel);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return werklijstPageClass;
	}

	protected InstellingGebruiker getIngelogdeGebruiker()
	{
		return ScreenitSession.get().getLoggedInInstellingGebruiker();
	}

	public void gaNaarBeoordeling(Long beoordelingId, AjaxRequestTarget target)
	{
		BeoordelingenReserveringResult reserveringResult = beoordelingService.openBeschikbareBeoordeling(beoordelingId, beoordelingenIds, getIngelogdeGebruiker(), getLezerSoort());

		if (reserveringResult.getInfoMessage() != null)
		{
			ScreenitSession.get().info(reserveringResult.getInfoMessage());
		}

		markeerFormulierenOpgeslagen(target);
		if (reserveringResult.getEersteGereserveerdeBeoordelingId() != null)
		{
			updateBeoordelingModel(reserveringResult);
			updateClientAfhankelijkePanels();
			updateImsOnderzoek(target);
			target.add(dossierContainer);
		}
		else
		{
			setResponsePage(werklijstPageClass);
		}
	}

	protected void updateImsOnderzoek(AjaxRequestTarget target)
	{
		target.prependJavaScript(createImsDesktopSyncCommand());
		target.prependJavaScript(createImsUpdateBsnCommand());
		target.prependJavaScript(createImsAllImagesSeenCommand());
	}

	public void volgendeBeoordeling(AjaxRequestTarget target)
	{
		zetBeoordeeldeLezingenInWerklijstfilter();

		Long volgendeBeoordelingId = beoordelingService.getVolgendeBeoordelingId(huidigeBeoordelingId(), beoordelingenIds);

		gaNaarBeoordeling(volgendeBeoordelingId, target);
	}

	private void zetBeoordeeldeLezingenInWerklijstfilter()
	{
		if (ScreenitSession.get().isZoekObjectGezetForComponent(werklijstPageClass))
		{
			IModel<MammaBeWerklijstZoekObject> zoekObjectModel = (IModel<MammaBeWerklijstZoekObject>) ScreenitSession.get().getZoekObject(werklijstPageClass);
			List<MammaBeoordelingStatus> filterStatussen = zoekObjectModel.getObject().getBeoordelingStatussen();
			if (filterStatussen.contains(MammaBeoordelingStatus.EERSTE_LEZING) && !filterStatussen.contains(MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN))
			{
				filterStatussen.add(MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN);
			}
			if (filterStatussen.contains(MammaBeoordelingStatus.TWEEDE_LEZING) && !filterStatussen.contains(MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN))
			{
				filterStatussen.add(MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN);
			}
			zoekObjectModel.getObject().setBeoordelingStatussen(filterStatussen);
			ScreenitSession.get().setZoekObject(werklijstPageClass, zoekObjectModel);
		}
	}

	protected abstract MammaBeLezerSoort getLezerSoort();

	protected String createImsDesktopSyncCommand()
	{
		String imsDesktopSyncMessage = imsService.createDesktopSyncMessage(getIngelogdeGebruiker().getMedewerker(), ScreenitSession.get().getMammaHuidigeIDS7Role(),
			huidigeOnderzoekId(), getVolgendeGereserveerdeBeoordelingenIds(), getMammobridgeFocusMode());
		return createUserSessionToImsBridgeSendCommand(MammaImsUserSessionType.ClientDesktopSync, imsDesktopSyncMessage, huidigeOnderzoekId());
	}

	protected MammobridgeFocusMode getMammobridgeFocusMode()
	{
		return MammobridgeFocusMode.ALLEEN_BVO_BEELDEN;
	}

	protected List<Long> getVolgendeGereserveerdeBeoordelingenIds()
	{
		return volgendeGereserveerdeBeoordelingenIds;
	}

	private String createImsOpenWebsocketCommand()
	{
		String gebruikersnaam = getIngelogdeGebruiker().getMedewerker().getGebruikersnaam();
		return "openWebsocketToImsBridge('" + gebruikersnaam + "');";
	}

	protected String createImsUpdateBsnCommand()
	{
		String bsn = getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient().getPersoon().getBsn();
		return "bsnVoorImsCheck = '" + bsn + "'";
	}

	private String createImsEmptyDesktopSyncCommand()
	{
		String imsDesktopSyncMessage = imsService.createEmptyDesktopSyncMessage(getIngelogdeGebruiker().getMedewerker(), ScreenitSession.get().getMammaHuidigeIDS7Role());
		return String.format("console.log('IMS: unload event'); if(!window.imsLogOffSent) {%s};",
			createUserSessionToImsBridgeSendCommand(MammaImsUserSessionType.EmptyDesktopSync, imsDesktopSyncMessage));
	}

	private String createEventListenerBeforeUnload()
	{
		return "window.addEventListener('beforeunload', function(e) {" + createImsEmptyDesktopSyncCommand() + "});";
	}

	protected String createImsAllImagesSeenCommand()
	{
		String allImagesSeenObject = imsService.createAllImagesSeenMessage(getIngelogdeGebruiker().getMedewerker(), ScreenitSession.get().getMammaHuidigeIDS7Role(),
			getOnderzoek(), getMammobridgeFocusMode());
		return "allImagesSeenRequestBody = " + allImagesSeenObject + ";";
	}

	protected void getHistorischeRondePanels(IModel<MammaBeoordeling> beoordelingModel, List<AbstractMammaRondePanel> rondePanels)
	{
		List<MammaBeoordeling> historischeBeoordelingen = beoordelingService.getAlleBeoordelingenMetBeelden(beoordelingModel.getObject());
		for (MammaBeoordeling beoordeling : historischeBeoordelingen)
		{
			MammaBeoordelenHistorischeRondePanel readOnlyRondePanel = new MammaBeoordelenHistorischeRondePanel("rondeItem", ModelUtil.sModel(beoordeling));
			rondePanels.add(readOnlyRondePanel);
		}
	}

	protected Class<? extends MammaScreeningBasePage> getWerklijstPageClass()
	{
		return werklijstPageClass;
	}
}
