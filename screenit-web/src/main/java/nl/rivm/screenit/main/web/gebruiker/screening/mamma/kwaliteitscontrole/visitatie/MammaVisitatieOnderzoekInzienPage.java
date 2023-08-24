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
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.service.mamma.MammaImsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaRondePanel;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;
import nl.rivm.screenit.model.mamma.enums.MammobridgeFocusMode;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaVisitatieOnderzoekInzienPage extends AbstractMammaBeoordelenPage
{

	@SpringBean
	private MammaImsService imsService;

	@SpringBean
	private HibernateService hibernateService;

	private final Map<Long, Long> onderzoekenIdMapping;

	private MammaVisitatieBesprekenHuidigeRondePanel huidigeRondePanel;

	public MammaVisitatieOnderzoekInzienPage(Long initieleBeoordelingId, Map<Long, Long> onderzoekenIdMapping, Class<? extends MammaScreeningBasePage> werklijstPageClass)
	{
		super(initieleBeoordelingId, new ArrayList<>(onderzoekenIdMapping.keySet()), werklijstPageClass);
		this.onderzoekenIdMapping = onderzoekenIdMapping;
	}

	@Override
	protected void maakRondesContainer(IModel<MammaBeoordeling> beoordelingModel)
	{
		List<AbstractMammaRondePanel> rondePanels = new ArrayList<>();

		MammaVisitatieOnderzoek visitatieOnderzoek = hibernateService.load(MammaVisitatieOnderzoek.class,
			onderzoekenIdMapping.get(beoordelingModel.getObject().getId()));
		huidigeRondePanel = new MammaVisitatieBesprekenHuidigeRondePanel("rondeItem", ModelUtil.sModel(beoordelingModel.getObject()),
			ModelUtil.sModel(visitatieOnderzoek));
		rondePanels.add(huidigeRondePanel);

		addRondeHistorie(rondePanels);
	}

	@Override
	protected MammaBeLezerSoort getLezerSoort()
	{
		return MammaBeLezerSoort.FOTOBESPREKING_VISITATIE;
	}

	@Override
	protected MammobridgeFocusMode getMammobridgeFocusMode()
	{
		return MammobridgeFocusMode.INCLUSIEF_UPLOAD_BEELDEN;
	}

	@Override
	protected void handleImsError(AjaxRequestTarget target, String errorMessage, Long onderzoekId)
	{
		error(imsService.handleError(errorMessage, ScreenitSession.get().getLoggedInInstellingGebruiker(), (b) -> getString((String) b), onderzoekId));
		huidigeRondePanel.blokeerButtons(target);
	}

	@Override
	protected Panel getClientPaspoortPanel(String id)
	{
		return new MammaClientPaspoortPanel(id, new PropertyModel<>(getModel(), "onderzoek.afspraak.uitnodiging.screeningRonde"), true);
	}

	@Override
	protected Panel getMiniWerklijst(String id)
	{
		return new MammaVisitatieMiniWerklijstPanel(id, this, onderzoekenIdMapping.get(huidigeBeoordelingId()), new ArrayList<>(onderzoekenIdMapping.values()));
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.visitatie.overzicht",
			MammaVisitatieOverzichtPage.class));
		contextMenuItems.addAll(MammaVisitatieOnderdeelWrapper.getContextMenuItems());

		return contextMenuItems;
	}
}
