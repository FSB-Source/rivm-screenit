package nl.rivm.screenit.mamma.se.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.mamma.se.dto.actions.ActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.TransactionDto;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;

import org.springframework.http.ResponseEntity;

public interface SeTransactionService
{
	ResponseEntity executeAsTransactionIfAuthorised(List<ActionDto> acties, TransactionDto transactionDto, LocalDateTime transactieDatumTijd,
		InstellingGebruiker transactieGebruiker,
		MammaScreeningsEenheid screeningsEenheid) throws IOException;
}
