package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.beelden;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractBEAccordionPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBeoordelenHistorischeRondePanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammobridgeFocusMode;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_BEELDEN_BEKIJKEN, Recht.GEBRUIKER_BEELDEN_PORTFOLIO })
public class MammaBeeldenInzienPage extends AbstractMammaBeoordelenPage
{
	@SpringBean
	private LogService logService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaBaseOnderzoekService onderzoekService;

	private IModel<MammaOnderzoek> laatsteOnderzoekModel;

	private final List<Long> clientenIds;

	private IModel<List<MammaOnderzoek>> onderzoekenModel;

	private IndicatingAjaxLink<Void> volgendeKnop;

	MammaBeeldenInzienPage(List<Long> clientenIds, List<MammaOnderzoek> onderzoeken, Class<? extends MammaScreeningBasePage> zoekPaginaClass)
	{
		super(null, null, zoekPaginaClass);
		this.clientenIds = clientenIds;

		onderzoekenModel = ModelUtil.listRModel(onderzoeken);
		laatsteOnderzoekModel = new SimpleHibernateModel<>(MammaOnderzoek.class, onderzoeken.get(0).getId());

		if (MammaPortfolioZoekenPage.class.equals(zoekPaginaClass))
		{
			volgendeKnop = maakVolgendeKnop();
			volgendeKnop.setOutputMarkupId(true);
			volgendeKnop.setOutputMarkupPlaceholderTag(true);
			dossierContainer.add(volgendeKnop);
		}
		else
		{
			dossierContainer.add(new EmptyPanel("volgende").setVisible(false));
		}
	}

	private IndicatingAjaxLink<Void> maakVolgendeKnop()
	{
		return new IndicatingAjaxLink<>("volgende")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				volgendeBeoordeling(target);
			}
		};
	}

	@Override
	protected void openInitieleBeoordeling(Long initieleOnderzoekId)
	{
		updateClientAfhankelijkePanelsVoorOnderzoek();
	}

	private void updateClientAfhankelijkePanelsVoorOnderzoek()
	{
		maakPaspoort();
		maakMiniWerklijst();
		maakRondesContainer();
	}

	private void maakRondesContainer()
	{
		List<AbstractBEAccordionPanel<?>> rondePanels = new ArrayList<>();
		boolean isFirstRound = true;
		for (MammaOnderzoek onderzoek : onderzoekenModel.getObject())
		{
			AbstractBEAccordionPanel<?> panel;
			if (onderzoek.getLaatsteBeoordeling() != null && MammaBeoordelingStatus.isUitslagStatus(onderzoek.getLaatsteBeoordeling().getStatus()))
			{
				panel = new MammaBeoordelenHistorischeRondePanel("rondeItem", ModelUtil.sModel(onderzoek.getLaatsteBeoordeling()));
			}
			else
			{
				panel = new MammaBeeldenInzienOnderzoekPanel("rondeItem", ModelUtil.sModel(onderzoek));
			}
			if (isFirstRound)
			{
				panel.setUseLazyLoading(false);
				panel.setIngeklapt(false);
				isFirstRound = false;
			}
			rondePanels.add(panel);
		}
		fillRondesInContainer(rondePanels);
	}

	@Override
	public void volgendeBeoordeling(AjaxRequestTarget target)
	{
		if (clientenIds.size() > 1)
		{
			clientenIds.remove(0);
			updateModels();
			updateClientAfhankelijkePanelsVoorOnderzoek();
			updateImsOnderzoek(target);
			target.add(dossierContainer);
		}
		else
		{
			setResponsePage(getWerklijstPageClass());
		}
	}

	private void updateModels()
	{
		Client volgendeClient = hibernateService.get(Client.class, clientenIds.get(0));
		List<MammaOnderzoek> volgendeOnderzoeken = onderzoekService.getOnderzoekenMetBeelden(volgendeClient);
		onderzoekenModel = ModelUtil.listRModel(volgendeOnderzoeken);
		laatsteOnderzoekModel = new SimpleHibernateModel<>(MammaOnderzoek.class, volgendeOnderzoeken.get(0).getId());
		logService.logGebeurtenis(LogGebeurtenis.INZIEN_BEELDEN_PORTFOLIO, ScreenitSession.get().getLoggedInAccount(), volgendeClient);
	}

	@Override
	public void gaNaarBeoordeling(Long onderzoekId, AjaxRequestTarget target)
	{
		throw new IllegalStateException("gaNaarBeoordeling niet ondersteund bij beelden inzien");
	}

	@Override
	protected Panel getClientPaspoortPanel(String id)
	{
		return new MammaClientPaspoortPanel(id, new PropertyModel<>(laatsteOnderzoekModel, "afspraak.uitnodiging.screeningRonde"), false);
	}

	@Override
	protected MammaOnderzoek getOnderzoek()
	{
		return ModelUtil.nullSafeGet(laatsteOnderzoekModel);
	}

	@Override
	protected MammobridgeFocusMode getMammobridgeFocusMode()
	{
		return MammobridgeFocusMode.INCLUSIEF_UPLOAD_BEELDEN;
	}

	@Override
	protected void maakRondesContainer(IModel<MammaBeoordeling> beoordelingModel)
	{
		throw new IllegalStateException("beoordeling niet aanwezig bij beelden bekijken, openInitieleBeoordeling moet overriden zijn/blijven");
	}

	@Override
	protected MammaBeLezerSoort getLezerSoort()
	{
		throw new IllegalStateException(
			"gaNaarBeoordeling en openInitieleBeoordeling zijn in deze class overridden, waardoor deze methode niet meer aangeroepen zou moeten worden.");
	}

	@Override
	protected void handleImsError(AjaxRequestTarget target, String errorMessage, Long onderzoekId)
	{
		error(imsService.handleError(errorMessage, getIngelogdeGebruiker(), b -> getString((String) b), onderzoekId));
		disableVolgendeKnop(target);
	}

	private void disableVolgendeKnop(AjaxRequestTarget target)
	{
		if (volgendeKnop != null)
		{
			volgendeKnop.setEnabled(false);
			target.add(volgendeKnop);
		}
	}

	@Override
	protected List<Long> getVolgendeGereserveerdeBeoordelingenIds()
	{
		return new ArrayList<>();
	}

	@Override
	protected Panel getMiniWerklijst(String id)
	{
		return new EmptyPanel(id);
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{

		return new ArrayList<>(Arrays.asList(
			new GebruikerMenuItem("label.tab.mammascreening.beelden.zoeken", MammaBeeldenZoekenPage.class),
			new GebruikerMenuItem("label.tab.mammascreening.portfolio.zoeken", MammaPortfolioZoekenPage.class)));
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(laatsteOnderzoekModel);
		ModelUtil.nullSafeDetach(onderzoekenModel);
	}
}
