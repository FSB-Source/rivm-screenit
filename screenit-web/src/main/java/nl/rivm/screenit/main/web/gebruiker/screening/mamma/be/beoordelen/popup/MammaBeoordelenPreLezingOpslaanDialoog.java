package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.beoordelen.popup;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.afbeelding.MammaSignaleringAfbeeldingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.beoordelen.MammaBeoordelenHuidigeRondePanel;
import nl.rivm.screenit.model.mamma.MammaLezing;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;

public abstract class MammaBeoordelenPreLezingOpslaanDialoog extends GenericPanel<MammaLezing>
{
	private final WebMarkupContainer mbbSignaleringContainer;

	private final WebMarkupContainer allImagesSeenSection;

	private boolean mbbSignalering = true;

	private boolean allImagesSeen = true;

	private Model<String> popupTitleModel;

	public MammaBeoordelenPreLezingOpslaanDialoog(String id, IModel<MammaLezing> model, MammaBeoordelenHuidigeRondePanel beoordelingPanel)
	{
		super(id, model);
		add(new AjaxLink<Void>("gezien")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});

		mbbSignaleringContainer = new WebMarkupContainer("mbbSignalering");
		mbbSignaleringContainer.setVisible(!mbbSignalering);
		mbbSignaleringContainer.add(new MammaSignaleringAfbeeldingPanel("mbbSignaleringAfbeelding", new PropertyModel<>(beoordelingPanel.getModel(), "onderzoek.signaleren")));
		add(mbbSignaleringContainer);

		maakRadioloogOpmerkingTextArea(beoordelingPanel);

		allImagesSeenSection = new WebMarkupContainer("allImagesSeenSection");
		allImagesSeenSection.setVisible(!allImagesSeen);
		add(allImagesSeenSection);

		popupTitleModel = Model.of("");
		updateTitle();
		Label popupTitleLabel = new Label("popupTitle", popupTitleModel);
		popupTitleLabel.setOutputMarkupId(true);
		add(popupTitleLabel);
	}

	public void updateTitle()
	{
		String popupTitle = (!allImagesSeen) ? "Nieuwe opnamen" : "";
		popupTitle += (!allImagesSeen && mbbSignalering) ? " & " : "";
		popupTitle += (mbbSignalering) ? "MBB Signalering" : "";
		popupTitleModel.setObject(popupTitle);
	}

	private void maakRadioloogOpmerkingTextArea(MammaBeoordelenHuidigeRondePanel beoordelingPanel)
	{
		final TextArea<String> opmerkingVoorRadioloog = new TextArea<>("opmerkingVoorRadioloog");
		opmerkingVoorRadioloog.setModel(Model.of(beoordelingPanel.getModelObject().getOnderzoek().getOpmerkingVoorRadioloog()));
		opmerkingVoorRadioloog.setEnabled(false);
		mbbSignaleringContainer.add(opmerkingVoorRadioloog);
	}

	public abstract void close(AjaxRequestTarget target);

	public boolean isMbbSignalering()
	{
		return mbbSignalering;
	}

	public void setMbbSignalering(boolean mbbSignalering)
	{
		this.mbbSignalering = mbbSignalering;
		mbbSignaleringContainer.setVisible(mbbSignalering);
	}

	public boolean isAllImagesSeen()
	{
		return allImagesSeen;
	}

	public void setAllImagesSeen(boolean allImagesSeen)
	{
		this.allImagesSeen = allImagesSeen;
		this.allImagesSeenSection.setVisible(!allImagesSeen);
	}
}
