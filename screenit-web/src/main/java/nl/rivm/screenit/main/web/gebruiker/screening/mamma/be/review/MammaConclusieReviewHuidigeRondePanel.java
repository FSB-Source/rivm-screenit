package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.review;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxButton;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.panels.MammaKwaliteitscontroleHuidigeRondePanel;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.jetbrains.annotations.NotNull;

public class MammaConclusieReviewHuidigeRondePanel extends MammaKwaliteitscontroleHuidigeRondePanel
{
	@SpringBean
	private MammaConclusieReviewService conclusieReviewService;

	private final IModel<MammaConclusieReview> conclusieReviewModel;

	private final IModel<MammaScreeningRonde> screeningRondeModel;

	private final boolean coordinerendRadioloogKijktBijAndereRadioloog;

	private final List<Long> beoordelingIds;

	public MammaConclusieReviewHuidigeRondePanel(String id, IModel<MammaBeoordeling> beoordelingModel, IModel<MammaConclusieReview> conclusieReviewModel,
		IModel<MammaScreeningRonde> screeningRondeModel, List<Long> beoordelingIds)
	{
		super(id, beoordelingModel);
		this.beoordelingIds = beoordelingIds;
		this.coordinerendRadioloogKijktBijAndereRadioloog = !conclusieReviewModel.getObject().getRadioloog().equals(ScreenitSession.get().getLoggedInInstellingGebruiker());
		this.screeningRondeModel = screeningRondeModel;

		this.conclusieReviewModel = coordinerendRadioloogKijktBijAndereRadioloog ?
			ModelUtil.ccModel(
				conclusieReviewService.getConclusieReviewCoordinerendRadioloog(screeningRondeModel.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker())) :
			conclusieReviewModel;

		setIngeklapt(false);
	}

	@Override
	protected void createButtons(WebMarkupContainer panelContainer, List<Component> buttons)
	{
		var form = new ScreenitForm<>("fotobespreking_form", conclusieReviewModel);
		var reviewAlGedaan = conclusieReviewModel.getObject().getReviewMoment() != null;

		createRedenenFotobesprekingVelden(form);

		var buttonGezien = new ScreenitIndicatingAjaxButton("gezien", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				voerSubmitUit(target);
			}

		};
		var radioloogReviewedEigenNietGereviewedeReview = !coordinerendRadioloogKijktBijAndereRadioloog && !reviewAlGedaan;
		buttonGezien.setVisible(radioloogReviewedEigenNietGereviewedeReview);

		form.add(buttonGezien);
		buttons.add(buttonGezien);

		var retourCe = maakRetourCeButton(form, radioloogReviewedEigenNietGereviewedeReview);
		retourCe.setVisible(radioloogReviewedEigenNietGereviewedeReview);

		form.add(retourCe);

		buttons.add(retourCe);

		var buttonOpslaan = new ScreenitIndicatingAjaxButton("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				voerSubmitUit(target);
			}
		};
		buttonOpslaan.setVisible(!coordinerendRadioloogKijktBijAndereRadioloog && reviewAlGedaan);
		form.add(buttonOpslaan);
		buttons.add(buttonOpslaan);

		var buttonVolgende = new ScreenitIndicatingAjaxButton("volgende", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				voerVolgendeUit(target);
			}
		};
		buttonVolgende.setVisible(coordinerendRadioloogKijktBijAndereRadioloog);
		form.add(buttonVolgende);
		buttons.add(buttonVolgende);

		panelContainer.add(form);
	}

	@NotNull
	private ScreenitIndicatingAjaxSubmitLink maakRetourCeButton(ScreenitForm<MammaConclusieReview> form, boolean radioloogReviewedEigenNietGereviewedeReview)
	{
		var retourCeButton = new ScreenitIndicatingAjaxSubmitLink("retourCe", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				dialog.openWith(target, new MammaConclusieReviewRetourCePopupPanel(IDialog.CONTENT_ID, conclusieReviewModel)
				{
					@Override
					public void opslaan(AjaxRequestTarget target)
					{
						dialog.close(target);
						var laatsteBeoordeling = screeningRondeModel.getObject().getLaatsteOnderzoek().getLaatsteBeoordeling();
						beoordelingIds.remove(laatsteBeoordeling.getId());
						voerSubmitUit(target);
					}
				});
			}
		};
		retourCeButton.setVisible(radioloogReviewedEigenNietGereviewedeReview);
		return retourCeButton;
	}

	private void createRedenenFotobesprekingVelden(ScreenitForm<MammaConclusieReview> form)
	{
		var redenenFotobesprekingRadioloogSelector = new ScreenitListMultipleChoice<>(
			"redenenFotobesprekingRadioloog",
			Arrays.asList(MammaLezingRedenenFotobesprekingRadioloog.values()),
			new EnumChoiceRenderer<>());
		form.add(redenenFotobesprekingRadioloogSelector);

		var redenenFotobesprekingMbberSelector = new ScreenitListMultipleChoice<>("redenenFotobesprekingMbber",
			Arrays.asList(MammaLezingRedenenFotobesprekingMbber.values()),
			new EnumChoiceRenderer<>());
		form.add(redenenFotobesprekingMbberSelector);
	}

	private void voerSubmitUit(AjaxRequestTarget target)
	{
		conclusieReviewService.conclusieReviewAfronden(conclusieReviewModel.getObject());

		logConclusieReviewAfgerond();

		((AbstractMammaBeoordelenPage) getPage()).volgendeBeoordeling(target);
	}

	private void voerVolgendeUit(AjaxRequestTarget target)
	{
		conclusieReviewService.saveConclusieReviewCoordinerendRadioloog(conclusieReviewModel.getObject(), screeningRondeModel.getObject(),
			ScreenitSession.get().getLoggedInInstellingGebruiker());

		((AbstractMammaBeoordelenPage) getPage()).volgendeBeoordeling(target);
	}

	private void logConclusieReviewAfgerond()
	{
		conclusieReviewService.logConclusieReviewAfgerond(ScreenitSession.get().getLoggedInInstellingGebruiker(), screeningRondeModel.getObject().getDossier().getClient(),
			conclusieReviewModel.getObject(),
			coordinerendRadioloogKijktBijAndereRadioloog);
	}

	@Override
	protected void onDetach()
	{
		ModelUtil.nullSafeDetach(conclusieReviewModel);
		ModelUtil.nullSafeDetach(screeningRondeModel);
		super.onDetach();
	}
}
