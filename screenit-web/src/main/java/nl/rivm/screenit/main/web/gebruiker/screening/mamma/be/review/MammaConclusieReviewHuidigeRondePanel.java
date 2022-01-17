package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.review;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxButton;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.panels.MammaKwaliteitscontroleHuidigeRondePanel;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaConclusieReviewHuidigeRondePanel extends MammaKwaliteitscontroleHuidigeRondePanel
{
	@SpringBean
	private MammaConclusieReviewService conclusieReviewService;

	@SpringBean
	private MammaBaseBeoordelingService beoordelingService;

	@SpringBean
	private LogService logService;

	private final IModel<MammaConclusieReview> conclusieReviewModel;

	public MammaConclusieReviewHuidigeRondePanel(String id, IModel<MammaBeoordeling> beoordelingModel, IModel<MammaConclusieReview> conclusieReviewModel)
	{
		super(id, beoordelingModel);
		this.conclusieReviewModel = conclusieReviewModel;
		setIngeklapt(false);
	}

	@Override
	protected void createButtons(WebMarkupContainer panelContainer, List<Component> buttons)
	{
		ScreenitForm<MammaConclusieReview> form = new ScreenitForm<>("fotobespreking_form", conclusieReviewModel);
		boolean reviewAlGedaan = conclusieReviewModel.getObject().getReviewMoment() != null;

		createRedenenFotobesprekingVelden(form);

		ScreenitIndicatingAjaxButton buttonGezien = new ScreenitIndicatingAjaxButton("gezien", form)
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				voerSubmitUit(target);
			}

		};
		buttonGezien.setVisible(!reviewAlGedaan);
		form.add(buttonGezien);
		buttons.add(buttonGezien);

		ScreenitIndicatingAjaxButton buttonOpslaan = new ScreenitIndicatingAjaxButton("opslaan", form)
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				voerSubmitUit(target);
			}
		};
		buttonOpslaan.setVisible(reviewAlGedaan);
		form.add(buttonOpslaan);
		buttons.add(buttonOpslaan);

		panelContainer.add(form);
	}

	private void createRedenenFotobesprekingVelden(ScreenitForm<MammaConclusieReview> form)
	{
		ScreenitListMultipleChoice<MammaLezingRedenenFotobesprekingRadioloog> redenenFotobesprekingRadioloogSelector = new ScreenitListMultipleChoice<>(
			"redenenFotobesprekingRadioloog",
			Arrays.asList(MammaLezingRedenenFotobesprekingRadioloog.values()),
			new EnumChoiceRenderer<>());
		form.add(redenenFotobesprekingRadioloogSelector);

		ScreenitListMultipleChoice<MammaLezingRedenenFotobesprekingMbber> redenenFotobesprekingMbberSelector = new ScreenitListMultipleChoice<>("redenenFotobesprekingMbber",
			Arrays.asList(MammaLezingRedenenFotobesprekingMbber.values()),
			new EnumChoiceRenderer<>());
		form.add(redenenFotobesprekingMbberSelector);
	}

	private void voerSubmitUit(AjaxRequestTarget target)
	{
		MammaConclusieReview conclusieReview = conclusieReviewModel.getObject();
		conclusieReviewService.conclusieReviewAfronden(conclusieReview);

		logService.logGebeurtenis(LogGebeurtenis.CONCLUSIE_REVIEW_AFGEROND, ScreenitSession.get().getLoggedInAccount(),
			beoordelingService.getClientVanBeoordeling(getModelObject()), Bevolkingsonderzoek.MAMMA);

		((AbstractMammaBeoordelenPage) getPage()).volgendeVerslag(target);
	}

	@Override
	protected void onDetach()
	{
		ModelUtil.nullSafeDetach(conclusieReviewModel);
		super.onDetach();
	}
}
