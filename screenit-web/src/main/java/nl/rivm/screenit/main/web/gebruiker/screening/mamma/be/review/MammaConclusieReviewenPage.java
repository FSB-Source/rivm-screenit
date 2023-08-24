package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.review;

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

import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaRondePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.panels.MammaKwaliteitscontroleHuidigeRondePanel;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;
import nl.rivm.screenit.model.mamma.enums.MammobridgeFocusMode;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaConclusieReviewenPage extends AbstractMammaBeoordelenPage
{

	@SpringBean
	private MammaConclusieReviewService conclusieReviewService;

	private final List<Long> beoordelingIds;

	private final IModel<InstellingGebruiker> radioloogModel;

	private MammaKwaliteitscontroleHuidigeRondePanel huidigeRondePanel;

	public MammaConclusieReviewenPage(Long initieleBeoordelingId, List<Long> beoordelingIds, Class<? extends MammaScreeningBasePage> werklijstPageClass,
		IModel<InstellingGebruiker> radioloogModel)
	{
		super(initieleBeoordelingId, beoordelingIds, werklijstPageClass);
		this.beoordelingIds = beoordelingIds;
		this.radioloogModel = radioloogModel;
	}

	@Override
	protected void maakRondesContainer(IModel<MammaBeoordeling> beoordelingModel)
	{
		List<AbstractMammaRondePanel> rondePanels = new ArrayList<>();
		MammaScreeningRonde screeningRonde = baseBeoordelingService.getScreeningRonde(beoordelingModel.getObject());

		MammaConclusieReview conclusieReview = conclusieReviewService.getConclusieReview(screeningRonde, radioloogModel.getObject());

		huidigeRondePanel = new MammaConclusieReviewHuidigeRondePanel("rondeItem", ModelUtil.sModel(beoordelingModel.getObject()), ModelUtil.ccModel(conclusieReview),
			ModelUtil.sModel(screeningRonde));
		rondePanels.add(huidigeRondePanel);

		addRondeHistorie(rondePanels);
	}

	@Override
	protected MammaBeLezerSoort getLezerSoort()
	{
		return MammaBeLezerSoort.CONCLUSIE_REVIEW;
	}

	@Override
	protected Panel getMiniWerklijst(String id)
	{
		return new MammaConclusieReviewMiniWerklijstPanel(id, this, huidigeBeoordelingId(), beoordelingIds, radioloogModel);
	}

	@Override
	protected MammobridgeFocusMode getMammobridgeFocusMode()
	{
		return MammobridgeFocusMode.INCLUSIEF_UPLOAD_BEELDEN;
	}

	@Override
	protected void handleImsError(AjaxRequestTarget target, String errorMessage, Long onderzoekId)
	{
		error(imsService.handleError(errorMessage, ScreenitSession.get().getLoggedInInstellingGebruiker(), b -> getString((String) b), onderzoekId));
		huidigeRondePanel.blokeerButtons(target);
	}

	@Override
	protected void logBeoordelingIngezien()
	{
		beoordelingService.logBeoordelingIngezien(getModel().getObject(), getIngelogdeGebruiker(),
			!ScreenitSession.get().getLoggedInInstellingGebruiker().equals(radioloogModel.getObject()));
	}

	@Override
	public void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(radioloogModel);
	}
}
