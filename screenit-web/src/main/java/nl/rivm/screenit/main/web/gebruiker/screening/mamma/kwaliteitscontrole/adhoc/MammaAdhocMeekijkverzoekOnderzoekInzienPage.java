package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.adhoc;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Map;

import nl.rivm.screenit.main.service.mamma.MammaImsService;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractBEAccordionPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaAdhocMeekijkverzoekOnderzoekInzienPage extends AbstractMammaBeoordelenPage
{

	@SpringBean
	private MammaImsService imsService;

	@SpringBean
	private HibernateService hibernateService;

	private final Map<Long, Long> onderzoekenIdMapping;

	private MammaAdhocMeekijkverzoekHuidigeRondePanel huidigeRondePanel;

	private IModel<MammaOnderzoek> onderzoekModel;

	public MammaAdhocMeekijkverzoekOnderzoekInzienPage(Long initieleOnderzoekId, Map<Long, Long> onderzoekenIdMapping,
		Class<? extends MammaScreeningBasePage> werklijstPageClass)
	{
		super(initieleOnderzoekId, new ArrayList<>(onderzoekenIdMapping.keySet()), werklijstPageClass);
		this.onderzoekenIdMapping = onderzoekenIdMapping;
	}

	@Override
	protected void openInitieleBeoordeling(Long initieleOnderzoekId)
	{
		openOnderzoek(initieleOnderzoekId);
	}

	private void openOnderzoek(Long initieleOnderzoekId)
	{
		onderzoekModel = new SimpleHibernateModel<>(MammaOnderzoek.class, initieleOnderzoekId);
		maakPaspoort();
		maakMiniWerklijst();
		maakRondesContainer();
	}

	private void maakRondesContainer()
	{
		List<AbstractBEAccordionPanel<?>> rondePanels = new ArrayList<>();

		MammaAdhocMeekijkverzoek adhocMeekijkverzoek = hibernateService.load(MammaAdhocMeekijkverzoek.class,
			onderzoekenIdMapping.get(getOnderzoek().getId()));
		huidigeRondePanel = new MammaAdhocMeekijkverzoekHuidigeRondePanel("rondeItem", onderzoekModel,
			ModelUtil.sModel(adhocMeekijkverzoek));
		rondePanels.add(huidigeRondePanel);

		fillRondesInContainer(rondePanels);
	}

	@Override
	public void volgendeVerslag(AjaxRequestTarget target)
	{
		Long volgendeOnderzoekId = beoordelingService.getVolgendeBeoordelingId(huidigeOnderzoekId(), new ArrayList<>(onderzoekenIdMapping.keySet()));

		gaNaarVerslag(volgendeOnderzoekId, target);
	}

	@Override
	public void gaNaarVerslag(Long onderzoekId, AjaxRequestTarget target)
	{
		if (onderzoekId != null)
		{
			openOnderzoek(onderzoekId);
			target.prependJavaScript(createImsDesktopSyncCommand());
			target.prependJavaScript(createImsUpdateBsnCommand());
			target.prependJavaScript(createImsAllImagesSeenCommand());
			target.add(dossierContainer);
		}
		else
		{
			setResponsePage(getActiveContextMenuClass());
		}
	}

	@Override
	protected MammaOnderzoek getOnderzoek()
	{
		return ModelUtil.nullSafeGet(onderzoekModel);
	}

	@Override
	protected void maakRondesContainer(IModel<MammaBeoordeling> beoordelingModel)
	{

		throw new IllegalStateException("beoordeling niet aanwezig bij ad hoc kwaliteitscontrole, openInitieleBeoordeling moet overriden zijn/blijven");
	}

	@Override
	protected MammaBeLezerSoort getLezerSoort()
	{
		throw new IllegalStateException("gaNaarVerslag en openInitieleBeoordeling zijn in deze class overridden, waardoor deze methode niet meer aangeroepen zou moeten worden.");
	}

	@Override
	protected void handleImsError(AjaxRequestTarget target, String errorMessage, Long onderzoekId)
	{
		error(imsService.handleError(errorMessage, getIngelogdeGebruiker(), (b) -> getString((String) b), onderzoekId));
		huidigeRondePanel.blokeerButtons(target);
	}

	@Override
	protected Panel getClientPaspoortPanel(String id)
	{
		return new EmptyPanel(id);
	}

	@Override
	protected Panel getMiniWerklijst(String id)
	{
		return new MammaAdhocMeekijkverzoekMiniWerklijstPanel(id, this, onderzoekenIdMapping.get(huidigeOnderzoekId()), new ArrayList<>(onderzoekenIdMapping.values()));
	}

	@Override
	protected List<Long> getVolgendeGereserveerdeBeoordelingenIds()
	{
		return new ArrayList<>();
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.adhockwaliteitscontrole.onderzoeken",
			MammaAdhocMeekijkverzoekOnderzoekenWerklijstPage.class));

		return contextMenuItems;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(onderzoekModel);
	}
}
